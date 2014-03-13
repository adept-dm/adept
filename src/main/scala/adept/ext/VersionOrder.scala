package adept.ext

import adept.resolution.models._
import adept.repository.models._
import adept.repository.GitRepository
import java.io.File
import adept.repository.serialization.VariantMetadata
import adept.logging.Logging
import adept.repository.serialization.Order
import java.io.FileWriter
import adept.repository.serialization.RepositoryMetadata

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
    var touchedFiles = {
      repository.listActiveOrderIds(id, commit).map { orderId =>
        repository.getOrderFile(id, orderId)
      } + repository.getOrderLookupFile(id)
    }

    allBinaryVersions.toSeq.sortBy { case (binaryVersion, _) => Version(binaryVersion) }.zipWithIndex.foreach {
      case ((binaryVersion, variants), index) =>
        val lines = variants.sortBy(getVersion).reverse.map { variant =>
          VariantMetadata.fromVariant(variant).hash.value
        }
        writeLines(lines, repository.getOrderFile(id, OrderId(index + 1)))
    }

    val lookupLines = (1 to (allBinaryVersions.size)).map(_.toString)
    writeLines(lookupLines, repository.getOrderLookupFile(id))

    val orderFiles = (1 to (allBinaryVersions.size)).map { orderId =>
      repository.getOrderFile(id, OrderId(orderId))
    }

    touchedFiles ++ orderFiles
  }

  def useBinaryVersionOf(id: Id, repository: GitRepository, commit: Commit, inRepositories: Set[GitRepository]): Set[GitRepository] = {
    val activeVariants = Order.activeVariants(id, repository, commit)

    val changedRepositories = inRepositories.flatMap { otherRepo =>
      val otherCommit = otherRepo.getHead
      otherRepo.listIds(otherCommit).flatMap { otherId =>
        val variants = Order.activeVariants(otherId, otherRepo, otherCommit)
        variants.flatMap { otherHash =>
          VariantMetadata.read(otherId, otherHash, otherRepo, otherCommit) match {
            case None => throw new Exception("Could not update binary version for: " + id + " in " + otherId + " because we could not find a variant for hash: " + otherHash + " in " + otherRepo + " and commit " + commit)
            case Some(otherVariant) =>
              val (targetRequirements, untouchedRequirements) = otherVariant.requirements
                .partition { r =>
                  r.id == id &&
                    !r.constraints.exists(_.name == AttributeDefaults.BinaryVersionAttribute) //skip the constraints that have 
                }
              val maybeBinaryVersion = RepositoryMetadata.read(otherId, otherHash, otherRepo, otherCommit) match {
                case Some(metadata) =>
                  val currentRepositoryInfos = metadata.repositories.filter(r => r.id == id && r.repository == repository.name)
                  if (currentRepositoryInfos.size > 1) throw new Exception("Aborting binary version update because we found more than 1 targer repositories for: " + id + " in " + metadata + ": " + currentRepositoryInfos)

                  val foundBinaryVersion = currentRepositoryInfos.headOption.flatMap { matchingRepositoryInfo =>
                    val maybeFoundVariant = VariantMetadata.read(matchingRepositoryInfo.id, matchingRepositoryInfo.variant,
                      repository, matchingRepositoryInfo.commit)
                    maybeFoundVariant match {
                      case Some(foundVariant) => getVersion(foundVariant).map(_.asBinaryVersion)
                      case None => throw new Exception("Aborting binary version update because we could not update required variant for: " + matchingRepositoryInfo + " in " + repository.dir)
                    }
                  }
                  foundBinaryVersion
                case None => throw new Exception("Could not update binary version for: " + id + " in " + otherId + " because we could not find a repository info for: " + otherHash + " in repo " + otherRepo.dir.getAbsolutePath + " commit " + otherCommit)
              }

              val fixedRequirements = for {
                requirement <- targetRequirements
                binaryVersion <- maybeBinaryVersion
              } yield {
                requirement.copy(constraints = requirement.constraints + Constraint(AttributeDefaults.BinaryVersionAttribute, Set(binaryVersion)))
              }

              if (fixedRequirements.nonEmpty) {
                val newMetadata = VariantMetadata
                  .fromVariant(otherVariant.copy(requirements = untouchedRequirements ++ fixedRequirements))
                otherRepo.add(newMetadata
                  .write(otherId, otherRepo))
                val oldHash = VariantMetadata.fromVariant(otherVariant).hash
                otherRepo.listActiveOrderIds(otherId, otherCommit).foreach { orderId =>
                  Order.replace(otherId, orderId, otherRepo, otherCommit) { currentHash =>
                    if (currentHash == oldHash) {
                      Some(Seq(newMetadata.hash, oldHash)) //place new metadata before old
                    } else None
                  }.foreach(otherRepo.add(_))
                }
                Some(otherRepo)
              } else {
                None
              }
          }
        }
      }
    }
    Set() ++ changedRepositories
  }
}
