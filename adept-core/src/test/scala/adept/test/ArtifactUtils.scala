package adept.test

import java.io.File
import adept.repository.models.RepositoryName
import adept.repository.models.Commit
import adept.artifact.models.ArtifactHash
import adept.repository.GitRepository
import adept.repository.metadata.ArtifactMetadata
import adept.artifact.models.Artifact
import org.scalatest.Matchers
import adept.resolution.models.Variant
import adept.artifact.ArtifactCache

object ArtifactUtils extends Matchers {
  def checkArtifactCache(baseDir: File, variant: Variant) = {
    assert(variant.artifacts.nonEmpty, "Could not find any artifacts for variant: " + variant) 
    variant.artifacts.foreach { artifact =>
      val cacheFile = ArtifactCache.getCacheFile(baseDir, artifact.hash, artifact.filename.getOrElse(throw new Exception("Expected a filename for: " + artifact + " did not find any. Variant: " + variant)))
      assert(cacheFile.isFile, "Could not find any files for: " + artifact + ". Expected it to be here: " + cacheFile.getAbsolutePath)
    }
  }
  def checkArtifactFilename(repository: GitRepository, variant: Variant) = {
    assert(variant.artifacts.nonEmpty, "Could not find any artifacts for variant: " + variant) 
    variant.artifacts.foreach { artifact =>
      val hash = artifact.hash
      val ending = artifact.filename.getOrElse {
        throw new Exception("Expected to be able to find a filename for: " + artifact + " in " + variant)
      }
      val commit = repository.getHead
      ArtifactMetadata.read(hash, repository, commit) match {
        case Some(a @ ArtifactMetadata(_, locations)) =>
          locations.foreach { location =>
            assert(location.endsWith(ending), "Found " + a + " for " + hash + " in repo: " + repository.dir.getAbsolutePath + " for commit: " + commit + " but it does not end with: " + ending)
          }
        case None =>
          assert(false, "Could not find an artifact for " + hash + " in repo: " + repository.dir.getAbsolutePath + " for commit: " + commit)
      }
    }
  }
}