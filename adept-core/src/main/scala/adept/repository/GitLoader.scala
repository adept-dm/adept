package adept.repository

import adept.resolution.models._
import adept.repository.models._
import adept.repository.metadata.VariantMetadata
import java.io.File
import net.sf.ehcache.CacheManager
import adept.hash.Hasher
import net.sf.ehcache.Ehcache
import org.eclipse.jgit.lib.ProgressMonitor
import adept.repository.metadata.ContextMetadata
import adept.logging.Logging
import adept.repository.metadata.RepositoryLocationsMetadata
import adept.repository.metadata.RankingMetadata
import org.eclipse.jgit.lib.NullProgressMonitor

object GitLoader extends Logging {

  private def hash(id: Id, constraints: Set[Constraint], uniqueId: String): String = {
    val uniqueString = "idc" + (id.value + constraints.toSeq.sorted.map(c => c.name + c.values.mkString(",").mkString(";")) + uniqueId)
    Hasher.hash(uniqueString.getBytes)
  }

  private val CacheName = "adept-cache"

  private def getCache(cacheManager: CacheManager): Ehcache = {
    cacheManager.addCacheIfAbsent(CacheName)
    cacheManager.getEhcache(CacheName)
  }

  private def hash(requirements: Set[(RepositoryName, Requirement, Commit)]): String = {
    val uniqueString = "rrc" + requirements.map {
      case (repository, requirement, commit) =>
        hash(requirement.id, requirement.constraints, repository.value + commit.value)
    }.toSeq.sorted.mkString("$")
    Hasher.hash(uniqueString.getBytes)
  }

  //TODO: private because I might want to move this to another class? 
  private[adept] def computeTransitiveContext(baseDir: File, context: Set[ContextValue], unversionedBaseDir: Option[File] = None): Set[ContextValue] = {
    context.flatMap { c =>
      (c.commit, unversionedBaseDir) match {
        case (Some(commit), _) =>
          val repository = new GitRepository(baseDir, c.repository)
          val children = ContextMetadata.read(c.id, c.variant, repository, commit).getOrElse(throw new Exception("Could not read context from: " + c))
          children.values
        case (None, Some(unversionedBaseDir)) =>
          val repository = new Repository(unversionedBaseDir, c.repository)
          val children = ContextMetadata.read(c.id, c.variant, repository).getOrElse(throw new Exception("Could not read context from: " + c))
          children.values
        case (None, None) =>
          throw new Exception("Found: " + c + " but both commit and unversioned base dir was None")
      }
    }
  }

  //TODO: private because I might want to move this to another class? 
  private[adept] def computeTransitiveLocations(baseDir: File, inputContext: Set[ContextValue], transitiveContext: Set[ContextValue], unversionedBaseDir: Option[File] = None): Set[RepositoryLocations] = {
    //TODO: UGLY code, refactor with computeTransitiveContext
    val requiredRepositories = transitiveContext.map(_.repository)

    inputContext.flatMap { c =>
      val values = (c.commit, unversionedBaseDir) match {
        case (Some(commit), _) =>
          val repository = new GitRepository(baseDir, c.repository)
          if (!repository.exists) throw new Exception("Cannot get transitive locations for: " + c)
          val children = ContextMetadata.read(c.id, c.variant, repository, commit).getOrElse(throw new Exception("Could not read locations from: " + c))
          children.values
        case (None, Some(unversionedBaseDir)) =>
          val repository = new Repository(unversionedBaseDir, c.repository)
          if (!repository.exists) throw new Exception("Cannot get transitive locations for: " + c)
          val children = ContextMetadata.read(c.id, c.variant, repository).getOrElse(throw new Exception("Could not read locations from: " + c))
          children.values
        case (None, None) =>
          throw new Exception("Found: " + c + " but both commit and unversioned base dir was None")
      }
      values.filter(v => requiredRepositories(v.repository)).flatMap { v =>
        (c.commit, unversionedBaseDir) match {
          case (Some(commit), _) =>
            val repository = new GitRepository(baseDir, c.repository)
            if (!repository.exists) throw new Exception("Cannot get transitive locations for: " + c)
            RepositoryLocationsMetadata.read(v.repository, repository, commit).map(_.toRepositoryLocations(v.repository)).toSet +  RepositoryLocations(repository.name, repository.getRemoteUri(GitRepository.DefaultRemote).toSet)
          case (None, Some(unversionedBaseDir)) =>
            val repository = new Repository(unversionedBaseDir, c.repository)
            if (!repository.exists) throw new Exception("Cannot get transitive locations for: " + c)
            RepositoryLocationsMetadata.read(v.repository, repository).map(_.toRepositoryLocations(v.repository)).toSet +  RepositoryLocations(repository.name, Set.empty)
          case (None, None) => throw new Exception("Found: " + c + " but both commit and unversioned base dir was None")
        }
      }
    }
  }

  //TODO: private because I might want to move this to another class? 
  private[adept] def applyOverrides(context: Set[ContextValue], overrides: Set[ContextValue]): Set[ContextValue] = {
    val overridesById = overrides.groupBy(v => (v.repository, v.id))
    context.flatMap { v =>
      overridesById.getOrElse(v.repository -> v.id, Set(v))
    }
  }

}

private[adept] class GitLoader(baseDir: File, private[adept] val context: Set[ContextValue], cacheManager: CacheManager, unversionedBaseDirs: Set[File] = Set.empty, private[adept] val loadedVariants: Set[Variant] = Set.empty, progress: ProgressMonitor = NullProgressMonitor.INSTANCE) extends VariantsLoader with Logging {
  import GitLoader._
  import adept.utils.CacheHelpers.usingCache

  private val thisUniqueId = Hasher.hash((
    context.map { cv => cv.id.value + "-" + cv.repository.value + "-" + cv.variant.value + "-" + cv.commit.map(_.value).mkString }.toSeq.sorted.mkString("#") ++
    loadedVariants.map(variant => VariantMetadata.fromVariant(variant).hash.value).toSeq.sorted.mkString("#")).getBytes)

  private val cache: Ehcache = getCache(cacheManager)

  private lazy val cachedById = { //lazy this might take a while
    context.groupBy(_.id).map {
      case (id, contextValues) =>
        val variantsLazyLoad = contextValues.groupBy(_.repository).flatMap {
          case (repositoryName, contextValues) =>
            val gitRepository = new GitRepository(baseDir, repositoryName)
            //use only latest commit:
            val allCommits = contextValues.collect {
              case ContextValue(_, _, Some(commit), _) => commit
            }
            val onlyLatestCommits = GitHelpers.lastestCommits(gitRepository, allCommits)
            val allVariants = contextValues.map(_.variant)

            //all rankings
            val gitRankings = onlyLatestCommits.flatMap { commit =>
              RankingMetadata
                .listRankIds(id, gitRepository, commit)
                .flatMap { rankId =>
                  RankingMetadata.read(id, rankId, gitRepository, commit).map(_.toRanking(id, rankId))
                }
            }

            val repositories = unversionedBaseDirs.map {
              new Repository(_, repositoryName)
            }
            val unversionedRankings = repositories.flatMap { repository =>
              RankingMetadata.listRankIds(id, repository).flatMap { rankId =>
                RankingMetadata.read(id, rankId, repository).map(_.toRanking(id, rankId))
              }
            }
            val allRankings = gitRankings ++ unversionedRankings
            if (allVariants.nonEmpty && allRankings.isEmpty) throw new Exception("Could not find any ranking files for: " + id + " when comparing: " + results)

            //choose variants to use:
            val chosenVariants = RankLogic.chosenVariants(allVariants, allRankings)
            if (allVariants.nonEmpty && chosenVariants.isEmpty) throw new Exception("Could not chose variants for: " + id + ". Variants: " + allVariants + "\nRankings: " + allRankings)

            val gitHashes = gitRankings.flatMap(_.variants)
            val unversionedHashes = unversionedRankings.flatMap(_.variants)

            //return variant lazy loaders for everything (might return None)
            chosenVariants.flatMap { variant =>
              if (gitHashes(variant) && !unversionedHashes(variant)) {
                onlyLatestCommits.map { commit =>
                  () => {
                    VariantMetadata.read(id, variant, gitRepository, commit, checkHash = true).map(_.toVariant(id))
                  }
                }
              } else if (unversionedHashes(variant) && !gitHashes(variant)) {
                logger.warn("Using unversioned (imported?) variant " + variant + " for " + id + " - contribute to git repositories to avoid this warning")
                repositories.map { repository =>
                  () => {
                    VariantMetadata.read(id, variant, repository, checkHash = true).map(_.toVariant(id))
                  }
                }
              } else if (unversionedHashes(variant) && gitHashes(variant)) {
                logger.warn("Using unversioned (imported?) variant: " + variant + " in " + repositories.map(_.dir.getAbsolutePath).mkString(",") + " and versioned ones from: " + gitRepository.dir.getAbsolutePath())
                repositories.map { repository =>
                  () => {
                    VariantMetadata.read(id, variant, repository, checkHash = true).map(_.toVariant(id))
                  }
                } ++ onlyLatestCommits.map { commit =>
                  () => {
                    VariantMetadata.read(id, variant, gitRepository, commit, checkHash = true).map(_.toVariant(id))
                  }
                }
              } else {
                throw new Exception("Expected to hash: " + variant + " to be in a rankig in either: " + repositories.map(_.dir.getAbsolutePath).mkString(",") + " or " + gitRepository.dir.getAbsolutePath() + ". Rankings:\n" + allRankings.mkString("\n"))
              }

            }
        }.toSet
        id -> variantsLazyLoad
    }
  }

  private lazy val preloadedById: Map[Id, Set[Variant]] = {
    loadedVariants.groupBy(_.id)
  }

  private def locateRepositoryIdentifiers(id: Id) = {
    cachedById.getOrElse(id, Set.empty)
  }

  def loadVariants(id: Id, constraints: Set[Constraint]): Set[Variant] = {
    val cacheKey = "loadVariants" + hash(id, constraints, thisUniqueId)
    usingCache(cacheKey, cache) {
      val repositoryVariants = locateRepositoryIdentifiers(id).flatMap { variantsLazyLoad =>
        variantsLazyLoad()
      }

      val variants = repositoryVariants ++ preloadedById.getOrElse(id, Set.empty)

      AttributeConstraintFilter.filter(id, variants, constraints)
    }
  }

}
