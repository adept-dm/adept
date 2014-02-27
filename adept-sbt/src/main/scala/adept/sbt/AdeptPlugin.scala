package adept.sbt

import sbt.{ Id => _, _ }
import sbt.Keys._
import sbt.complete.DefaultParsers._
import sbt.complete._
import play.api.libs.json.Json
import adept.repository.AdeptGitRepository
import adept.models._
import adept.repository.models.configuration.ConfiguredRequirement
import adept.repository.AdeptCommit
import adept.resolution.Resolver
import adept.repository.GitVariantsLoader
import adept.repository.models.Commit
import adept.repository.models.configuration.ConfiguredRequirement
import adept.repository.models.configuration.ConfigurationId
import net.sf.ehcache.CacheManager
import adept.repository.models.RepositoryMetadata
import adept.repository.models.configuration.ConfiguredRequirement
import adept.resolution.models.UnderconstrainedResult
import adept.resolution.models.ResolvedResult
import adept.resolution.models.OverconstrainedResult
import adept.resolution.models.ResolveResult
import adept.artifacts.ArtifactCache
import adept.models.Hash
import adept.repository.models.configuration.ConfigurationId

object AdeptRepository {
  import sbt.complete.DefaultParsers._

  def repositories = (Faked.baseDir / "repos").listFiles().filter(_.isDirectory).map(_.getName)

  def modules(repoName: String) = {
    (Faked.baseDir / "repos" / repoName / "variants").listFiles().filter(_.isDirectory).map(_.getName)
  }

  def repositoryParser: Parser[String] = {
    val candidates = repositories.toList.sorted
    if (candidates.size == 1) {
      candidates.head ^^^ candidates.head
    } else if (candidates.size > 1) {
      candidates.tail.foldLeft(candidates.head ^^^ candidates.head) { (currentParser, candidate) =>
        (candidate ^^^ candidate | currentParser)
      }
    } else {
      Parser.failure("No repositories found")
    }
  }

  def idParser(repository: String): Parser[String] = {
    import sbt.complete.DefaultParsers._
    val candidates = modules(repository).toList.sorted
    if (candidates.size == 1) {
      candidates.head ^^^ candidates.head
    } else if (candidates.size > 1) {
      candidates.tail.foldLeft(candidates.head ^^^ candidates.head) { (currentParser, candidate) =>
        (candidate ^^^ candidate | currentParser)
      }
    } else {
      Parser.failure("No ids found")
    }
  }
}

abstract class AdeptCommand {
  def execute(): Unit
}

class IvyImportCommand(org: String, name: String, revision: String) extends AdeptCommand {
  def execute(): Unit = {
    println("ivy import: " + org + ", " + name + ", " + revision)
  }
}

class SetAdeptCommand(repo: String, conf: String, id: String, constraints: Set[(String, Seq[String])])(adeptManager: AdeptManager) extends AdeptCommand {
  import sbt.complete.DefaultParsers._

  def execute(): Unit = {
    adeptManager.set(repo, conf, id, constraints)
  }
}

package lockfile {
  package object serialization {
    implicit val configurationEntryFormat = Json.format[ConfigurationEntry]
    implicit val requirementEntryFormat = Json.format[RequirementEntry]
    implicit val artifactEntryFormat = Json.format[ArtifactEntry]
    implicit val lockfileFormat = Json.format[LockFile]
  }
  case class ConfigurationEntry(id: String, constraints: Set[Map[String, Seq[String]]], repository: String, commit: String)
  case class RequirementEntry(hash: String, configurations: Map[String, Seq[ConfigurationEntry]])

  case class ArtifactEntry(hash: String, variant: String, locations: Seq[String], size: Long)
  case class LockFile(requirements: Seq[RequirementEntry], artifacts: Seq[ArtifactEntry]) {
    def getResolveInfo: (Set[ConfiguredRequirement], Set[AdeptCommit]) = {
      ???
    }
  }
}

object LockFile {

  def read(file: File) = {
    if (file.exists() && file.isFile) {
      val source = io.Source.fromFile(file)
      import lockfile.serialization._
      Json.parse(source.getLines.mkString("\n")).as[lockfile.LockFile]
    } else lockfile.LockFile(Seq.empty, Seq.empty)
  }
}

//TODO: evaluate if Metadata and other files should all have Seq or Set. The argument for Seq is to make things more deterministic.
case class LockFileRequirement(id: Id, configuration: ConfigurationId, constraints: Seq[Constraint], repositoryName: String, repositoryCommit: Commit) {
  def asRequirement: Requirement = {
    Requirement(ConfigurationId.join(id, configuration), constraints.toSet)
  }
}

object Faked { //REMOVE THIS when finished testing (used for hard coding)
  val baseDir = Path.userHome / ".adept"

  var fakeRequirements = Set.empty[LockFileRequirement]

  val cacheManager = CacheManager.create
  var result: Option[ResolveResult] = None

  def artifacts = {
    result match {
      case Some(result) =>
        val variants = result.state.implicitVariants ++ result.state.resolvedVariants
        variants.flatMap { case (_, variant) => variant.artifacts.map(a => ArtifactCache.getArtifactCacheFile(baseDir, a.hash) -> a.filename) }.toSeq
      case None => Seq.empty[(File, String)]
    }
  }
}

class AdeptManager(baseDir: File, lockFile: File) {
  val DefaultConfigurations = Set(ConfigurationId("compile"), ConfigurationId("master"))

  def set(repo: String, conf: String, idString: String, constraints: Set[(String, Seq[String])]) = {
    val id = Id(idString)

    val initTime = System.currentTimeMillis

    val thisGitRepo = new AdeptGitRepository(baseDir, repo)

    val currentConfigurations = DefaultConfigurations //TODO: replace with setting
    val parsedConstraints = constraints.map { case (name, values) => Constraint(name, values.toSet) }

    val newReqs = currentConfigurations.map { configuration =>
      LockFileRequirement(id, configuration, parsedConstraints.toSeq, thisGitRepo.name, thisGitRepo.getMostRecentCommit.commit) //TODO: we should not use most recent commit but the first one that matches the constraints
    }

    val requirements = Faked.fakeRequirements.filter(_.id != id) ++ newReqs

    val initCommits = requirements.map { req =>
      req -> (ConfigurationId.join(req.id, req.configuration), AdeptCommit(new AdeptGitRepository(baseDir, req.repositoryName), req.repositoryCommit))
    } ++ requirements.map{ req =>
      req -> (req.id, AdeptCommit(new AdeptGitRepository(baseDir, req.repositoryName), req.repositoryCommit))
    }

    val commits: Set[(Id, AdeptCommit)] = {
      //TODO: adjust this ugly piece of code!
      val gitVariantsLoader = new GitVariantsLoader(initCommits.map(_._2), cacheManager = Faked.cacheManager)

      val rootVariantHashes = gitVariantsLoader.loadVariants(id, parsedConstraints).map(Hash.calculate)
      initCommits.flatMap {
        case (req, (commitId, c)) =>
          val repositoryMetadata = c.repo.listContent(c.commit.value).repositoryMetadata.filter { repositoryMetadata =>
             repositoryMetadata.variants.exists(h => rootVariantHashes.contains(h)) //i.e. there is repository variant represents one or more of the rootVariants
          }
          repositoryMetadata
            .flatMap(r => r.load(baseDir, req.id, req.configuration)) //grab all repository metadata for this repository
      } ++ initCommits.map(_._2)
    }

    val gitVariantsLoader = new GitVariantsLoader(commits, cacheManager = Faked.cacheManager)
    val gitResolver = new Resolver(gitVariantsLoader)

    val result = gitResolver.resolve(requirements.map(_.asRequirement))

    val resultString =
      result match {
        case _: ResolvedResult =>
          "Resolved (" + (System.currentTimeMillis - initTime) + "ms)"
        case _: OverconstrainedResult =>
          val requiredIds = newReqs.map(_.id)
          val currentOverconstrained = result.state.overconstrained.filter(requiredIds)
          val displayErrorIds = if (currentOverconstrained.isEmpty) result.state.overconstrained else currentOverconstrained
          val help = displayErrorIds.map { id =>
            if (gitVariantsLoader.loadVariants(id, result.state.constraints(id)).isEmpty) {
              if (gitVariantsLoader.loadVariants(id, Set.empty).isEmpty) {
                id + " cannot be found in repositories: " + commits.map{ case (id, c) => c.repo.name }.mkString(" or ")
              } else {
                id + result.state.constraints(id).map(c => c.name + "=" + c.values.mkString("(", ",", ")")).mkString(" with ", " and ", " does not exist")
              }
            } else {
              id + " conflicts " + result.state.constraints(id).map(c => c.name + "=" + c.values.mkString("(", ",", ")")).mkString(",")
            }
          }.mkString("\n")
          "Over-constrained (" + (System.currentTimeMillis - initTime) + "ms):\n" + help
        case underConstrainedResult: UnderconstrainedResult =>
          val help = "Choose between:\n" + (if (underConstrainedResult.optimalStates.nonEmpty) {
            underConstrainedResult.optimalStates.map(s => (s.implicitVariants ++ s.resolvedVariants).flatMap {
              case (foundId, foundVariant) =>
                if (result.state.underconstrained(foundId)) Some(foundVariant)
                else None
            }.toSet): Set[Set[Variant]]
          } else {
            result.state.underconstrained.map(id => gitVariantsLoader.loadVariants(id, parsedConstraints)): Set[Set[Variant]]
          }).map(_.map(_.attributes.map(a => a.name + "=" + a.values.mkString("(", ",", ")")).mkString("<", ",", ">")).mkString(" OR ")).mkString("\n")
          "Under-constrained (" + (System.currentTimeMillis - initTime) + "ms): " + result.state.underconstrained.mkString(",") + ":\n" + help
      }
    println(resultString)

    if (result.isResolved) { //TODO: I am not sure whether it is right to only store result if resolvd (if we are under-constrained it would be nice to increase precision..)
      Faked.fakeRequirements = requirements
    }
    Faked.result = Some(result)

  }
}

object AdeptPlugin extends Plugin {

  import AdeptKeys._

  def adeptSettings = Seq(
    adeptDirectory := Path.userHome / ".adept",
    adeptClasspath := {
      Faked.artifacts.map {
        case (file, name) =>
          file
      }
    },
    sbt.Keys.commands += {

      val SetCommand = token("set")
      val GraphCommand = token("graph")
      val IvyImport = token("ivy-import")

      val RepositorySep = token("/")
      val adeptManager = new AdeptManager(adeptDirectory.value, new File("adept.lock"))

      val repositoires = token(Space ~> AdeptRepository.repositoryParser) flatMap { repo =>
        token(RepositorySep ~> AdeptRepository.idParser(repo)).flatMap { id =>
          token((Space ~> charClass(_ => true, "").*) | charClass(_ => true, "").*).map { binaryVersionsChars =>
            val binaryVersionsString = binaryVersionsChars.mkString
            val binaryVersion = binaryVersionsString.trim().split(" ").map(_.trim()).filter(_.nonEmpty)
            val constraints =
              if (binaryVersion.isEmpty)
                Set.empty[(String, Seq[String])]
              else
                Set("binary-version" -> binaryVersion.toSeq)

            new SetAdeptCommand(repo, "compile", id, constraints)(adeptManager)
          }
        }
      }

      val set = SetCommand ~> repositoires

      val ivyImport = (IvyImport ~> (Space ~> charClass(_ => true, "ivy-organization").+.flatMap { orgChars =>
        val org = orgChars.mkString
        token("#") ~> charClass(_ => true, "ivy-name").+.flatMap { nameChars =>
          val name = nameChars.mkString
          token(";") ~> charClass(_ => true, "ivy-revision").+.map { revisionChars =>
            val revision = revisionChars.mkString
            new IvyImportCommand(org, name, revision)
          }
        }
      }))

      val graph = GraphCommand.map { _ =>
        new AdeptCommand {
          def execute() = {
            println(Faked.result match {
              case Some(result) => result.graphAsString
              case None => ""
            })
          }
        }

      }
      val adept = (Space ~> (set | ivyImport | graph))

      Command("adept")(_ => adept) { (state, adeptCommand) =>
        adeptCommand.execute()
        state
      }
    })
}