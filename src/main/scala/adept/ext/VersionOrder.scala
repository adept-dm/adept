package adept.ext

import adept.resolution.models.Variant
import adept.logging.Logging
import adept.repository.GitRepository
import adept.repository.models.Commit
import adept.repository.serialization.VariantMetadata
import adept.repository.models.VariantSet

object VersionOrder extends Logging {
  import adept.ext.AttributeDefaults._
  def isBinaryCompatible(variant1: Variant, variant2: Variant) = {
    variant1.id == variant2.id && {
      variant1.attribute(BinaryVersionAttribute) == variant2.attribute(BinaryVersionAttribute) //TODO: check if there is ONE binary version that matches ONE other. here we check all
    }
  }

  def getVersion(variant: Variant) = {
    variant.attributes.find { attribute =>
      attribute.name == VersionAttribute
    }.map { attribute =>
      if (attribute.values.size == 1) {
        Some(Version(attribute.values.head))
      } else {
        logger.warn("Did not find EXACTLY one version. Found: " + variant)
        None
      }
    }
  }

  def hasHigherVersion(variant1: Variant, variant2: Variant) = {
    variant1.id == variant2.id && {
      val res = for {
        version1 <- getVersion(variant1)
        version2 <- getVersion(variant2)
      } yield {
        version1 > version2
      }
      res.getOrElse(false)
    }
  }

  /** Used by Order to map versions and binary-versions to correct order */
  def versionReplaceLogic(variant: Variant, repository: GitRepository, commit: Commit)(variantSet: VariantSet) = {
    val newVariantMetadata = VariantMetadata.fromVariant(variant)

    //vars and loops are easier to read here (in my eyes) than a fold 
    var foundBinaryIncompatible = false
    var insertOnly = false
    variantSet.hashes.foreach { hash =>
      VariantMetadata.read(variant.id, hash, repository, commit) match {
        case Some(foundVariant) =>
          val currentBinaryCompatible = isBinaryCompatible(foundVariant, variant)
          if (currentBinaryCompatible) {
            insertOnly = insertOnly || hasHigherVersion(foundVariant, variant)
          } else {
            foundBinaryIncompatible = true
          }
        case _ => false
      }
    }

    if (insertOnly) {
      Some(Seq(variantSet, VariantSet(Set(newVariantMetadata.hash))))
    } else {
      if (foundBinaryIncompatible) {
        Some(Seq(variantSet.copy(hashes = variantSet.hashes + newVariantMetadata.hash)))
      } else {
        None
      }
    }
  }
}