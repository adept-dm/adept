package adept.ext

import java.io.File
import adept.resolution.models.Id
import adept.repository.models.RepositoryName
import adept.repository.models.VariantHash
import adept.repository.models.Commit
import adept.repository.GitRepository
import adept.repository.serialization.VariantMetadata
import adept.resolution.models.Attribute
import adept.resolution.models.Requirement
import adept.repository.models.RepositoryLocations
import adept.repository.serialization.RepositoryLocationsMetadata
import adept.repository.serialization.ResolutionResultsMetadata
import adept.repository.models.Ranking
import adept.repository.serialization.RankingMetadata
import adept.repository.RankLogic
import adept.repository.models.ResolutionResult
import adept.repository.serialization.ArtifactMetadata
import adept.artifact.models.Artifact
import adept.repository.Repository
import adept.repository.models.RankId

object VariantRename {
  private case class VariantAssociations(variant: VariantMetadata, resolutionResults: Set[ResolutionResult], repositoryLocations: Set[RepositoryLocations], artifactLocations: Set[Artifact])
  private def getAllVariantAssociations(id: Id, variant: VariantHash, repository: GitRepository, commit: Commit): Option[VariantAssociations] = {
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

  def rename(baseDir: File, sourceId: Id, sourceName: RepositoryName, sourceCommit: Commit, destId: Id, destName: RepositoryName): ((GitRepository, Set[File]), (GitRepository, Set[File])) = { //source repo and files, dest repo and files
    val sourceRepository = new GitRepository(baseDir, sourceName)
    val destRepository = new GitRepository(baseDir, destName)
    if (!destRepository.exists) throw new Exception("Could not rename to " + destName + " because repository: " + destRepository.exists + " is not initialized")

    val activeSourceVariantsRankIds = RankingMetadata.listRankIds(sourceId, sourceRepository, sourceCommit).flatMap { rankId =>
      RankingMetadata.read(sourceId, rankId, sourceRepository, sourceCommit).flatMap(ranking => ranking.variants.headOption).map(_ -> rankId)
    }
    val destRankIds = RankingMetadata.getXRankId(destId, destRepository, 0, activeSourceVariantsRankIds.size)

    assert(activeSourceVariantsRankIds.size == destRankIds.size)

    var sourceFiles = Set.empty[File]
    var destFiles = Set.empty[File]

    val redirectAttribute = Attribute("redirect", Set(sourceName.value + Repository.IdDirSep + sourceId.value + ":" + destName.value + Repository.IdDirSep + destId.value))
    val redirectRequirement = Requirement(destId, constraints = Set.empty, Set.empty)
    val redirectMetadata = VariantMetadata(
      attributes = Seq(redirectAttribute),
      artifacts = Seq.empty,
      requirements = Seq(redirectRequirement))

    //gather files:
    destRepository.getRemoteUri(GitRepository.DefaultRemote).foreach { uri =>
      sourceFiles += RepositoryLocationsMetadata(Seq(uri)).write(destRepository.name, sourceRepository)
    }

    def addIfNonExisting(id: Id, hash: VariantHash, rankId: RankId, repository: GitRepository, commit: Commit) = {
      val formerRankings = RankingMetadata.read(id, rankId, repository, commit).getOrElse(RankingMetadata(Seq.empty))
      if (!formerRankings.variants.exists(_ == hash)) {
        Some(RankingMetadata(hash +: formerRankings.variants).write(id, rankId, repository))
      } else None
    }

    (activeSourceVariantsRankIds zip destRankIds).foreach {
      case ((sourceHash, sourceRankId), destRankId) =>
        val VariantAssociations(variant, resolutionResults, repositoryLocations, artifactLocations) =
          getAllVariantAssociations(sourceId, sourceHash, sourceRepository, sourceCommit)
            .getOrElse(throw new Exception("Could not find associated data for: " + sourceHash + " in " + sourceId + " in repo: " + sourceRepository.dir.getAbsolutePath + " for " + sourceCommit))

        val newSourceVariant = variant.copy(attributes = variant.attributes :+ redirectAttribute, requirements = variant.requirements.filter(_.id != destId) :+ redirectRequirement)
        destFiles += newSourceVariant.write(destId, destRepository)
        destFiles += ResolutionResultsMetadata(resolutionResults.toSeq).write(destId, variant.hash, destRepository)
        destFiles ++= repositoryLocations.map(repositoryLocation => RepositoryLocationsMetadata(repositoryLocation.uris.toSeq).write(repositoryLocation.name, destRepository))
        destFiles ++= artifactLocations.map(artifact => ArtifactMetadata.fromArtifact(artifact).write(artifact.hash, destRepository))
        destFiles ++= addIfNonExisting(destId, newSourceVariant.hash, destRankId, destRepository, destRepository.getHead)

        sourceFiles += redirectMetadata.write(sourceId, sourceRepository)
        sourceFiles ++= addIfNonExisting(sourceId, redirectMetadata.hash, sourceRankId, sourceRepository, sourceCommit)
    }

    val oldDestRankId = {
      val formerSourceDestActiveVariants = RankLogic.getActiveVariants(sourceId, destRepository, destRepository.getHead)
      if (formerSourceDestActiveVariants.nonEmpty && formerSourceDestActiveVariants != Set(redirectMetadata.hash)) throw new Exception("Cannot rename because " + sourceId + " in " + destRepository.dir.getAbsolutePath + " exists for: " + destRepository.getHead + " and " + formerSourceDestActiveVariants + " is not equal to: " + Seq(redirectMetadata.hash))
      else RankingMetadata.getXRankId(sourceId, destRepository, 0, 1).head
    }
    destFiles += RankingMetadata(Seq(redirectMetadata.hash)).write(sourceId, oldDestRankId, destRepository)
    destFiles += redirectMetadata.write(sourceId, destRepository)

    (sourceRepository, sourceFiles) -> (destRepository, destFiles)
  }
}