package adept.ext

import adept.resolution.models._
import adept.repository.models._
import adept.repository.GitRepository
import java.io.File
import adept.repository.serialization.VariantMetadata
import adept.logging.Logging
import adept.repository.serialization.Order
import java.io.FileWriter
import adept.repository.serialization.ResolutionResultsMetadata
import scala.util.matching.Regex

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

object VersionOrder extends Logging {
  import adept.ext.AttributeDefaults._

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

  def writeLines(lines: Seq[String], file: File) = {
    var writer: FileWriter = null
    try {
      val append = false
      writer = new FileWriter(file, append)
      lines.foreach { line =>
        writer.write((line + '\n'))
      }
      writer.flush()
    } finally {
      if (writer != null) writer.close()
    }
  }

  def orderBinaryVersions(id: Id, repository: GitRepository, commit: Commit): Set[File] = {
    val variants = repository.listVariants(id, commit).map { hash =>
      VariantMetadata.read(id, hash, repository, commit) match {
        case Some(variant) => variant
        case _ => throw new Exception("Unexpectly could not read variant: " + hash + " in  " + repository.dir.getAbsolutePath)
      }
    }
    var allBinaryVersions = Map.empty[String, Seq[Variant]]
    variants.foreach { variant =>
      val binaryVersions = variant.attribute(BinaryVersionAttribute).values
      binaryVersions.foreach { binaryVersion =>
        val parsedVariants = allBinaryVersions.getOrElse(binaryVersion, Seq.empty)
        allBinaryVersions += binaryVersion -> (variant +: parsedVariants)
      }
    }
    val orders = repository.getXOrderId(id, 0, allBinaryVersions.size) //overwrites former files
    val newOrderIds = {
      ((0 to orders.size) zip orders).toMap
    }

    val oldOrderIds = repository.listActiveOrderIds(id, commit).diff(orders)
    val oldOrderFiles = oldOrderIds.map { orderId =>
      val orderFile = repository.getOrderFile(id, orderId)
      writeLines(Seq.empty, repository.getOrderFile(id, orderId)) //delete
      orderFile
    }

    val orderFiles = allBinaryVersions.toSeq.sortBy { case (binaryVersion, _) => Version(binaryVersion) }.zipWithIndex.map {
      case ((binaryVersion, variants), index) =>
        val lines = variants.sortBy(getVersion).reverse.map { variant =>
          VariantMetadata.fromVariant(variant).hash.value
        }
        val orderId = newOrderIds(index)
        val orderFile = repository.getOrderFile(id, orderId)
        writeLines(lines, orderFile)
        orderFile
    }

    orderFiles.toSet ++ oldOrderFiles.toSet
  }

  def useSemanticVersions(id: Id, hash: VariantHash, repository: GitRepository, commit: Commit, excludes: Set[Regex] = Set.empty, useVersionAsBinary: Set[Regex] = Set.empty): Set[File] = {
    val variant = VariantMetadata.read(id, hash, repository, commit)
      .getOrElse(throw new Exception("Could not find variant: " + id + " hash: " + hash + " in " + repository.dir.getAbsolutePath + " for " + commit))
    val versions = variant.attribute(AttributeDefaults.VersionAttribute).values
    val existingBinaryVersions = variant.attribute(AttributeDefaults.BinaryVersionAttribute).values
    if (versions.size == 1 && existingBinaryVersions.isEmpty) {
      val version = versions.head
      val exclude = excludes.exists { pattern =>
        pattern.findFirstIn(version).isDefined
      }
      if (exclude) {
        Set.empty
      } else {
        val useVersionAsBinaryHere = useVersionAsBinary.exists { pattern =>
          pattern.findFirstIn(version).isDefined
        }
        val binaryVersion = if (useVersionAsBinaryHere) {
          version
        } else {
          Version(version).asBinaryVersion
        }
        val attributes = variant.attributes + Attribute(AttributeDefaults.BinaryVersionAttribute, Set(binaryVersion))
        val changedFiles = replaceVariant(variant, variant.copy(attributes = attributes), repository, commit)
        changedFiles
      }
    } else {
      if (versions.size != 1)
        logger.debug("Skipping semantic version on " + id + " hash: " + hash + " in " + repository.dir.getAbsolutePath + " for " + commit + " because more than 1 version exists: " + versions)
      else if (existingBinaryVersions.nonEmpty)
        logger.debug("Skipping semantic version on " + id + " hash: " + hash + " in " + repository.dir.getAbsolutePath + " for " + commit + " because it already has a binary version: " + existingBinaryVersions)
      Set.empty
    }
  }

  private def replaceVariant(currentVariant: Variant, newVariant: Variant, repository: GitRepository, commit: Commit) = {
    val newMetadata = VariantMetadata.fromVariant(newVariant)
    val oldHash = VariantMetadata.fromVariant(currentVariant).hash
    val changedFiles = repository.listActiveOrderIds(currentVariant.id, commit).flatMap { orderId =>
      Order.replace(currentVariant.id, orderId, repository, commit) { currentHash =>
        if (currentHash == oldHash) {
          Some(Seq(newMetadata.hash, oldHash)) //place new hash before old
        } else None
      }
    } + newMetadata.write(newVariant.id, repository)
    changedFiles
  }

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
        val maybeFoundVariant = VariantMetadata.read(matchingRepositoryInfo.id, matchingRepositoryInfo.variant,
          repository, matchingRepositoryInfo.commit)
        val foundVariant = maybeFoundVariant.getOrElse(throw new Exception("Aborting binary version update because we could not update required variant for: " + matchingRepositoryInfo + " in " + repository.dir))
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
      otherRepo.listIds(otherCommit).flatMap { otherId =>
        val variants = Order.activeVariants(otherId, otherRepo, otherCommit)
        variants.flatMap { otherHash =>
          val otherVariant = VariantMetadata.read(otherId, otherHash, otherRepo, otherCommit).getOrElse(throw new Exception("Could not update binary version for: " + id + " in " + otherId + " because we could not find a variant for hash: " + otherHash + " in " + otherRepo + " and commit " + commit))
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
