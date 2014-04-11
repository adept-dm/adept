package adept.ext

import adept.repository.metadata.VariantMetadata
import adept.repository.GitRepository
import java.io.File
import adept.repository.metadata.ResolutionResultsMetadata

object MetadataUpdate {

  /** Update all resolution results that references the repository itself (Useful after semantic versioning) */
  def updateRepositoryResolutionResults(repository: GitRepository): Set[File] = {
    if (!repository.exists) throw new Exception("Cannot update internal resolution results for repository that does not exists: " + repository.dir.getAbsolutePath())
    val commit = repository.getHead
    val repositoryIds = VariantMetadata.listIds(repository, commit)
    var changedFiles = Set.empty[File]
    repositoryIds.foreach { id =>
      VariantMetadata.listVariants(id, repository, commit).foreach { hash =>
        ResolutionResultsMetadata.read(id, hash, repository, commit).foreach { metadata =>
          val oldValues = metadata.values
          val newValues = metadata.values.map { r =>
            if (repositoryIds(r.id)) r.copy(commit = commit)
            else r
          }
          changedFiles += ResolutionResultsMetadata(newValues).write(id, hash, repository)
        }
      }
    }
    changedFiles
  }

}