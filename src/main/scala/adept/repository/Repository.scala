package adept.repository

import java.io.File
import org.eclipse.jgit.lib.Constants
import adept.repository.models._
import adept.resolution.models._
import adept.repository.serialization.VariantMetadata
import adept.artifact.models.ArtifactHash
import java.io.InputStream
import org.apache.ivy.plugins.repository.file.FileRepository

case class InitException(repo: Repository, reason: String) extends Exception("Could not initialize '" + repo.dir.getAbsolutePath + "': " + reason)
case class MalformedVariantHashException(repo: Repository, hash: VariantHash) extends Exception("Variant hash: '" + hash.value + "' was not well-formed in repository: " + repo.dir.getAbsolutePath)
case class MalformedArtifactHashException(repo: Repository, hash: ArtifactHash) extends Exception("Artifact hash: '" + hash.value + "' was not well-formed in repository: " + repo.dir.getAbsolutePath)

object Repository {
  val ArtifactsMetadataDirName = "artifacts"
  val VariantsMetadataDirName = "variants"
  val ReposDirName = "repos"

  val JsonFileEnding = "json"
  val RepositoryMetadataFileName = "repositories." + JsonFileEnding
  val InfoMetadataFileName = "info." + JsonFileEnding
  val VariantMetadataFileName = "variant." + JsonFileEnding
  val OrderMetadataFileName = "order"


  val IdDirSep = "/" //the character in an ID that indicates a different directory

  val HashLength = 64
  val Level1Length = 4
  val Level2Length = 4
  val Level3Length = HashLength - Level1Length - Level2Length

  def getReposDir(baseDir: File) = new File(baseDir, ReposDirName)
  def getRepoDir(baseDir: File, name: String) = new File(getReposDir(baseDir), name)
  def getArtifactsMetadataDir(baseDir: File, name: String) = new File(getRepoDir(baseDir, name), ArtifactsMetadataDirName)
  def getVariantsMetadataDir(baseDir: File, name: String) = new File(getRepoDir(baseDir, name), VariantsMetadataDirName)
}

/**
 * Defines the Adept Repository file layout. 
 * Knowledge of file locations is required for WRITE operations. 
 * See [[adept.repository.GitRepository]] should be used for READ operations.
 *
 * Layout is defined as following:
 * <id> is the id and might be more than one sub-directory (foo/bar/zoo has 3 directory levels)
 * <hash> is the hash the item and is split into 2 sub directories (first 4 chars (level 1), next 4 chars (level 2), then the rest (level 3))
 * <repository name> is - you guessed it! - the repository name
 *
 * "repos"
 *   - <repository name>
 *     - "variants"
 *       - <id>
 *         - "order"
 *         - <hash>
 *           - "variant.json"
 *           - "repositories.json"
 *           - "info.json"
 *     - "locations"
 *       - "hosts.properties"
 *       - repositories
 *         - <repository name>
 *           - "repository.json"
 *       - artifacts
 *         - <hash>
 *           - "artifact.json"
 */
private[adept] class Repository(val baseDir: File, val name: RepositoryName) {
  import Repository._
  require(name.value.nonEmpty, "Cannot create a repository with an empty name")

  val dir = getRepoDir(baseDir, name.value)

  val artifactsMetadataDir = getArtifactsMetadataDir(baseDir, name.value)
  val variantsMetadataDir = getVariantsMetadataDir(baseDir, name.value)

  private def getVariantHashDir(id: Id, hash: VariantHash) = {
    if (hash.value.size != (Level1Length + Level2Length + Level3Length))
      throw MalformedVariantHashException(this, hash)
    else {
      val level1 = new File(getIdFile(variantsMetadataDir, id), hash.value.slice(0, Level1Length))
      val level2 = new File(level1, hash.value.slice(Level2Length, Level1Length + Level2Length))
      val level3 = new File(level2, hash.value.slice(Level1Length + Level2Length, Level3Length))
      level3
    }
  }

  private def ensureParentDirs(file: File) = {
    val dir = file.getParentFile
    if (!(dir.isDirectory() || dir.mkdirs())) throw InitException(this, "Could not create dir: " + dir.getAbsolutePath)
    else file
  }

  private def getIdFile(rootDir: File, id: Id): File = {
    id.value.split(IdDirSep).foldLeft(rootDir) { (currentPath, dir) =>
      new File(currentPath, dir)
    }
  }

  def getVariantFile(id: Id, hash: VariantHash) = {
    ensureParentDirs(new File(getVariantHashDir(id, hash), VariantMetadataFileName))
  }

  def getRepositoryFile(id: Id, hash: VariantHash) = {
    ensureParentDirs(new File(getVariantHashDir(id, hash), RepositoryMetadataFileName))
  }

  def getArtifactFile(hash: ArtifactHash) = {
    if (hash.value.size != (Level1Length + Level2Length + Level3Length))
      throw MalformedArtifactHashException(this, hash)
    else {
      val level1 = new File(artifactsMetadataDir, hash.value.slice(0, Level1Length))
      val level2 = new File(level1, hash.value.slice(Level1Length, Level2Length))
      ensureParentDirs(new File(level2, hash.value.slice(Level2Length, Level3Length)))
    }
  }

  def getOrderFile(id: Id) = {
    ensureParentDirs(new File(getIdFile(variantsMetadataDir, id), OrderMetadataFileName))
  }

}