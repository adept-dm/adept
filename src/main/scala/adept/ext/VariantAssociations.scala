package adept.ext

import adept.repository.metadata.VariantMetadata
import adept.repository.models.ResolutionResult
import adept.repository.models.RepositoryLocations
import adept.artifact.models.Artifact
import adept.resolution.models.Id
import adept.repository.models.VariantHash
import adept.repository.GitRepository
import adept.repository.models.Commit
import adept.repository.metadata.ResolutionResultsMetadata
import adept.repository.metadata.RepositoryLocationsMetadata
import adept.repository.metadata.ArtifactMetadata

private[ext] case class VariantAssociations(variant: VariantMetadata, resolutionResults: Set[ResolutionResult], repositoryLocations: Set[RepositoryLocations], artifactLocations: Set[Artifact])

private[ext] object VariantAssociations {
  def getAllVariantAssociations(id: Id, variant: VariantHash, repository: GitRepository, commit: Commit): Option[VariantAssociations] = {
    for {
      variantMetadata <- VariantMetadata.read(id, variant, repository, commit)
      resolutionResultsMetadata = ResolutionResultsMetadata.read(id, variant, repository, commit).getOrElse(ResolutionResultsMetadata(Seq()))
      repositoryLocations = {
        resolutionResultsMetadata.values.flatMap { resolutionResult =>
          val name = resolutionResult.repository
          RepositoryLocationsMetadata.read(name, repository, commit).map(_.toRepositoryLocations(name))
        }
      }
      artifactLocations = {
        variantMetadata.artifacts.flatMap { artifactRef =>
          ArtifactMetadata.read(artifactRef.hash, repository, commit).map(_.toArtifact(artifactRef.hash))
        }
      }
    } yield {
      VariantAssociations(
        variant = variantMetadata,
        resolutionResults = resolutionResultsMetadata.values.toSet,
        repositoryLocations = repositoryLocations.toSet,
        artifactLocations = artifactLocations.toSet)
    }
  }
}