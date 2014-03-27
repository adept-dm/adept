package adept.ivy

import adept.resolution.models._
import adept.artifact.models._
import java.io.File
import org.apache.ivy.core.module.id.ModuleRevisionId
import adept.repository.models._
import adept.ext.Version
import org.apache.ivy.core.module.descriptor.ExcludeRule

//Contains everything needed to be able to import from Ivy to Adept
case class IvyImportResult(variant: Variant, artifacts: Set[Artifact], localFiles: Map[ArtifactHash, File], repository: RepositoryName, versionInfo: Set[(RepositoryName, Id, Version)], excludeRules: Map[(Id, Id), Set[ExcludeRule]])

//Exceptions (thrown by Adept's Ivy import):
class IvyImportError(msg: String) extends Exception(msg)
case class AdeptIvyResolveException(msg: String) extends Exception(msg)
case class AdeptIvyException(msg: String) extends Exception(msg)

//Errors (returned in Either objects so caller can recover from them):
case class ArtifactLocationError(location: String, file: File) extends IvyImportError("Could not determine artifact location: " + location + " for file: '" + file.getAbsolutePath + "'. Deleting the file might help.")
case class IvyVerificationError(mismatchOnHash: ArtifactHash, variant: Variant, matchingHashes: Set[ArtifactHash])
case class IvyVerificationErrorReport(msg: String, adeptExtraArtifacts: Map[ArtifactHash, Variant], ivyExtraArtifacts: Map[ArtifactHash, ModuleRevisionId], nonMatching: Set[IvyVerificationError]) {
  override def toString = {
    msg + ".\n" +
      (if (adeptExtraArtifacts.nonEmpty) {
        "Artifacts found in Adept, but not in Ivy:\n" + adeptExtraArtifacts.map {
          case (hash, variant) =>
            "\t" + hash + " in " + variant
        }.mkString("", "\n", "\n")
      } else "\n") +
      (if (ivyExtraArtifacts.nonEmpty) {
        "Artifacts found in Ivy, but not in Adept:\n" + ivyExtraArtifacts.map {
          case (hash, mrid) =>
            "\t" + hash + " in " + mrid
        }.mkString("", "\n", "\n")
      } else "\n") +
      (if (nonMatching.nonEmpty) {
        "Found a variant for the artifact, but not the right artifact(s):\n" + nonMatching.map {
          case IvyVerificationError(hash, variant, matchingHashes) =>
            "\t" + hash + " in " + variant + (if (matchingHashes.nonEmpty) " found matching hashes:\n" +
              "\t\t" + matchingHashes.mkString(",")
            else " no matching hashes:\n") +
              (if (variant.artifacts.nonEmpty) "\t among variant artifacts:\n" +
                "\t\t" + variant.artifacts.mkString(",")
              else " NO variant artifacts!")
        }.mkString("\n")
      } else "\n")
  }
}