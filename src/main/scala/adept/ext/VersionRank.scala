package adept.ext

import adept.resolution.models._
import adept.repository.models._
import adept.repository.GitRepository
import java.io.File
import adept.repository.serialization.VariantMetadata
import adept.logging.Logging
import java.io.FileWriter
import adept.repository.serialization.ResolutionResultsMetadata
import scala.util.matching.Regex
import adept.repository.serialization.RankingMetadata
import adept.repository.RankLogic

//import adept.logging.Logging
//import adept.repository.GitRepository
//import adept.repository.models.Commit
//import adept.repository.serialization.VariantMetadata
//import adept.repository.models.VariantSet
//
//object VersionOrder extends Logging {
//  import adept.ext.AttributeDefaults._
//  def isBinaryCompatible(variant1: Variant, variant2: Variant) = {
//    variant1.id == variant2.id && {
//      variant1.attribute(BinaryVersionAttribute) == variant2.attribute(BinaryVersionAttribute) //TODO: check if there is ONE binary version that matches ONE other. here we check all
//    }
//  }
//
//  def getVersion(variant: Variant) = {
//    variant.attributes.find { attribute =>
//      attribute.name == VersionAttribute
//    }.map { attribute =>
//      if (attribute.values.size == 1) {
//        Some(Version(attribute.values.head))
//      } else {
//        logger.warn("Did not find EXACTLY one version. Found: " + variant)
//        None
//      }
//    }
//  }
//
//  def hasHigherVersion(variant1: Variant, variant2: Variant) = {
//    variant1.id == variant2.id && {
//      val res = for {
//        version1 <- getVersion(variant1)
//        version2 <- getVersion(variant2)
//      } yield {
//        version1 > version2
//      }
//      res.getOrElse(false)
//    }
//  }
//
//  /** Used by Order to map versions and binary-versions to correct order */
//  def versionReplaceLogic(variant: Variant, repository: GitRepository, commit: Commit)(variantSet: VariantSet) = {
//    val newVariantMetadata = VariantMetadata.fromVariant(variant)
//
//    //vars and loops are easier to read here (in my eyes) than a fold 
//    var foundBinaryIncompatible = false
//    var insertOnly = false
//    variantSet.hashes.foreach { hash =>
//      VariantMetadata.read(variant.id, hash, repository, commit) match {
//        case Some(foundVariant) =>
//          val currentBinaryCompatible = isBinaryCompatible(foundVariant, variant)
//          if (currentBinaryCompatible) {
//            insertOnly = insertOnly || hasHigherVersion(foundVariant, variant)
//          } else {
//            foundBinaryIncompatible = true
//          }
//        case _ => false
//      }
//    }
//
//    if (insertOnly) {
//
//      Some(Seq(variantSet, VariantSet(Set(newVariantMetadata.hash))))
//    } else {
//      if (foundBinaryIncompatible) {
//        Some(Seq(variantSet.copy(hashes = variantSet.hashes + newVariantMetadata.hash)))
//      } else {
//        None
//      }
//    }
//  }
//}

case class BinaryVersionUpdateException(msg: String) extends Exception(msg)
case class VersionNotFoundException(targetName: RepositoryName, targetId: Id, targetVersion: Version) extends Exception("Could not find version: " + targetVersion + " for id: " + targetId + " in repository:" + targetName)
case class RepositoryNotFoundException(targetName: RepositoryName, targetId: Id, targetVersion: Version) extends Exception("Could not find repository: " + targetName + " for id: " + targetId + " and version: " +  targetVersion)

object VersionRank extends Logging {
  import adept.ext.AttributeDefaults._

  def createResolutionResults(baseDir: File, versionInfo: Set[(RepositoryName, Id, Version)]): Set[ResolutionResult] = {
    val results = versionInfo.map {
      case (targetName, targetId, targetVersion) =>
        val repository = new GitRepository(baseDir, targetName)
        if (repository.exists) {
          val commit = repository.getHead
          VersionScanner.findVersion(targetId, targetVersion, repository, commit) match {
            case Some(targetHash) =>
              val result = ResolutionResult(targetId, targetName, commit, targetHash)
              result
            case None => throw VersionNotFoundException(targetName, targetId, targetVersion)
          }
        } else throw RepositoryNotFoundException(targetName, targetId, targetVersion)
    }
    results
  }

  def getVersion(variant: Variant) = {
    variant.attributes.find { attribute =>
      attribute.name == VersionAttribute
    }.flatMap { attribute =>
      if (attribute.values.size == 1) {
        Some(Version(attribute.values.head))
      } else {
        logger.warn("Did not find EXACTLY one version. Found: " + variant)
        None
      }
    }
  }

  private[adept] def getSortedByVersions(variants: Seq[Variant]): Seq[VariantHash] = {
    val hashes = variants.sortBy(getVersion).reverse.map { variant =>
      VariantMetadata.fromVariant(variant).hash
    }
    hashes
  }

  /** Creates new order files (and deletes the contents of old) according to 1) binary versions and 2) versions */
  def useSemanticVersionRanking(id: Id, repository: GitRepository, commit: Commit, excludes: Set[Regex] = Set.empty, useVersionAsBinary: Set[Regex] = Set.empty): (Set[File], Set[File]) = {

    //1) Get variants
    val variants = VariantMetadata.listVariants(id, repository, commit).map { hash =>
      VariantMetadata.read(id, hash, repository, commit) match {
        case Some(variantMetadata) => variantMetadata.toVariant(id)
        case _ => throw new Exception("Unexpectly could not read variant: " + hash + " in  " + repository.dir.getAbsolutePath)
      }
    }

    //2) Find binary versions or add new variant with binary versions
    var newVariants = Set.empty[File]
    var newResolutionResults = Set.empty[File]
    var allBinaryVersions = Map.empty[String, Seq[Variant]]

    val NoBinaryVersion = ""
    variants.foreach { variant =>
      val binaryVersions = variant.attribute(BinaryVersionAttribute).values
      if (binaryVersions.nonEmpty) {
        binaryVersions.foreach { binaryVersion =>
          val parsedVariants = allBinaryVersions.getOrElse(binaryVersion, Seq.empty)
          allBinaryVersions += binaryVersion -> (parsedVariants :+ variant)
        }
      } else {
        val versions = variant.attribute(VersionAttribute).values
        if (versions.size == 1) { //found a version but no binaryVersion, we add a binary version
          val version = Version(versions.head)
          val exclude = excludes.exists { pattern =>
            pattern.findFirstIn(version.value).isDefined
          }
          if (exclude) {
            val parsedVariants = allBinaryVersions.getOrElse(NoBinaryVersion, Seq.empty)
            allBinaryVersions += NoBinaryVersion -> (parsedVariants :+ variant)
          } else {
            val useVersionAsBinaryHere = useVersionAsBinary.exists { pattern =>
              pattern.findFirstIn(version.value).isDefined
            }
            val binaryVersion = if (useVersionAsBinaryHere) {
              version.value
            } else {
              version.asBinaryVersion
            }

            val newVariant = variant.copy(attributes = variant.attributes + Attribute(BinaryVersionAttribute, Set(binaryVersion)))
            val newVariantMetadata = VariantMetadata.fromVariant(newVariant)
            newVariants += newVariantMetadata.write(id, repository)
            newResolutionResults ++= ResolutionResultsMetadata.read(id, VariantMetadata.fromVariant(variant).hash, repository, commit).map {
              _.write(id, newVariantMetadata.hash, repository)
            }
            val parsedVariants = allBinaryVersions.getOrElse(binaryVersion, Seq.empty)
            allBinaryVersions += binaryVersion -> (parsedVariants :+ variant :+ newVariant) //add new variant first (last, it will be reversed), but also old variant so it can be located later
          }
        } else if (binaryVersions.isEmpty) {
          val parsedVariants = allBinaryVersions.getOrElse(NoBinaryVersion, Seq.empty)
          allBinaryVersions += NoBinaryVersion -> (parsedVariants :+ variant)
        }
      }
    }

    //3) Get rankings
    val rankingSize = allBinaryVersions.size
    val rankings = RankingMetadata.getXRankId(id, repository, 0, rankingSize) //overwrites former files
    assert(rankings.size == rankingSize)
    val newRankIds = {
      ((0 to rankings.size) zip rankings.toSeq.map(_.value).sorted).toMap
    }
    val oldRankingFiles = {
      RankingMetadata.listRankIds(id, repository, commit).diff(rankings).map(rankId => repository.getRankingFile(id, rankId))
    }

    //4) Write variants to ranking files 
    val rankingFiles = allBinaryVersions.toSeq.sortBy { case (binaryVersion, _) => Version(binaryVersion) }.zipWithIndex.map {
      case ((binaryVersion, variants), index) =>
        val rankId = RankId(newRankIds(index))
        val sortedVariants = getSortedByVersions(variants).distinct
        RankingMetadata(sortedVariants).write(id, rankId, repository)
    }
    (newVariants ++ newResolutionResults ++ rankingFiles.toSet, oldRankingFiles.toSet)
  }

  private def replaceVariant(currentVariant: Variant, newVariant: Variant, repository: GitRepository, commit: Commit) = {
    val newMetadata = VariantMetadata.fromVariant(newVariant)
    val oldHash = VariantMetadata.fromVariant(currentVariant).hash
    val id = currentVariant.id
    val changedFiles = RankingMetadata.listRankIds(id, repository, commit).flatMap { rankId =>
      RankingMetadata.read(id, rankId, repository, commit).flatMap { rankings =>
        if (rankings.variants.contains(oldHash)) {
          val (before, after) = rankings.variants.span(_ != oldHash)
          Some(RankingMetadata((before :+ newMetadata.hash) ++ after).write(id, rankId, repository))
        } else None
      }
    }

    changedFiles + newMetadata.write(newVariant.id, repository)
  }

  /** For variants that have binary-versions set in (id and repository), find all variants that requires it (in inRepositories) and lock the requirements to this binary-version */
  def useBinaryVersionOf(id: Id, repository: GitRepository, commit: Commit, inRepositories: Set[GitRepository]): Set[(GitRepository, File)] = {
    def getBinaryVersionRequirements(variant: Variant, resolutionResults: ResolutionResultsMetadata) = {
      val (targetRequirements, untouchedRequirements) = variant.requirements
        .partition { r =>
          r.id == id &&
            !r.constraints.exists(_.name == AttributeDefaults.BinaryVersionAttribute) //skip the constraints that already have binary versions
        }

      val currentResults = resolutionResults.values.filter(r => r.id == id && r.repository == repository.name)
      if (currentResults.size > 1) throw new Exception("Aborting binary version update because we found more than 1 target repositories for: " + id + " in " + resolutionResults + ": " + currentResults)

      val maybeBinaryVersion = currentResults.headOption.flatMap { matchingRepositoryInfo =>
        val foundVariant = {
          val maybeMetadata = VariantMetadata.read(matchingRepositoryInfo.id, matchingRepositoryInfo.variant,
            repository, matchingRepositoryInfo.commit)
          val metadata = maybeMetadata.getOrElse(throw new Exception("Aborting binary version update because we could not update required variant for: " + matchingRepositoryInfo + " in " + repository.dir))
          metadata.toVariant(matchingRepositoryInfo.id)
        }
        getVersion(foundVariant).map(_.asBinaryVersion)
      }

      val fixedRequirements = for {
        requirement <- targetRequirements
        binaryVersion <- maybeBinaryVersion
      } yield {
        requirement.copy(constraints = requirement.constraints + Constraint(AttributeDefaults.BinaryVersionAttribute, Set(binaryVersion)))
      }
      fixedRequirements -> untouchedRequirements
    }

    val changedFiles = inRepositories.par.flatMap { otherRepo =>
      val otherCommit = otherRepo.getHead
      VariantMetadata.listIds(otherRepo, otherCommit).flatMap { otherId =>
        val variants = RankLogic.getActiveVariants(otherId, otherRepo, otherCommit)
        variants.flatMap { otherHash =>
          val otherVariant = {
            val metadata = VariantMetadata.read(otherId, otherHash, otherRepo, otherCommit).getOrElse(throw new Exception("Could not update binary version for: " + id + " in " + otherId + " because we could not find a variant for hash: " + otherHash + " in " + otherRepo + " and commit " + commit))
            metadata.toVariant(otherId)
          }
          val resolutionResults = ResolutionResultsMetadata.read(otherId, otherHash, otherRepo, otherCommit).getOrElse(throw new Exception("Could not update binary version for: " + id + " in " + otherId + " because we could not find a repository info for: " + otherHash + " in repo " + otherRepo.dir.getAbsolutePath + " commit " + otherCommit))
          val (fixedRequirements, untouchedRequirements) = getBinaryVersionRequirements(otherVariant, resolutionResults)

          if (fixedRequirements.nonEmpty) {
            val newVariant = otherVariant.copy(requirements = untouchedRequirements ++ fixedRequirements)
            replaceVariant(otherVariant, newVariant, otherRepo, otherCommit).map(otherRepo -> _)
          } else Set.empty[(GitRepository, File)]
        }
      }
    }
    Set() ++ changedFiles
  }
}
