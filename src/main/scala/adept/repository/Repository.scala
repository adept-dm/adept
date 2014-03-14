package adept.repository

import java.io.File
import org.eclipse.jgit.lib.Constants
import adept.repository.models._
import adept.resolution.models._
import adept.repository.serialization.VariantMetadata
import adept.artifact.models.ArtifactHash
import java.io.InputStream
import org.apache.ivy.plugins.repository.file.FileRepository
import java.io.FileReader
import java.io.FilenameFilter

case class InitException(reason: String) extends Exception("Could not initialize: " + reason)
case class MalformedVariantHashException(repo: Repository, hash: VariantHash) extends Exception("Variant hash: '" + hash.value + "' (size: "+hash.value.length+") was not well-formed in repository: " + repo.dir.getAbsolutePath)
case class MalformedArtifactHashException(repo: Repository, hash: ArtifactHash) extends Exception("Artifact hash: '" + hash.value + "' was not well-formed in repository: " + repo.dir.getAbsolutePath)

object Repository {
  val ArtifactsMetadataDirName = "artifacts"
  val VariantsMetadataDirName = "variants"
  val ReposDirName = "repos"

  val JsonFileEnding = "json"
  val ResolutionResultsFileName = "resolution-results." + JsonFileEnding
  val InfoMetadataFileName = "info." + JsonFileEnding
  val VariantMetadataFileName = "variant." + JsonFileEnding
  val OrderFileNamePrefix = "order-"

  val IdDirSep = "/" //the character in an ID that indicates a different directory

  val HashLength = 64
  val Level1Length = 4
  val Level2Length = 4
  val Level3Length = HashLength - Level1Length - Level2Length

  def getReposDir(baseDir: File) = new File(baseDir, ReposDirName)
  def getRepoDir(baseDir: File, name: RepositoryName) = new File(getReposDir(baseDir), name.value)
  def getArtifactsMetadataDir(baseDir: File, name: RepositoryName) = new File(getRepoDir(baseDir, name), ArtifactsMetadataDirName)
  def getVariantsMetadataDir(baseDir: File, name: RepositoryName) = new File(getRepoDir(baseDir, name), VariantsMetadataDirName)

  private[adept] def ensureParentDirs(file: File) = {
    val dir = file.getParentFile
    if (!(dir.isDirectory() || dir.mkdirs())) throw InitException("Could not create dir: " + dir.getAbsolutePath)
    else file
  }
}

/**
 * Defines the Adept Repository file layout.
 * Knowledge of file locations is required for read and write operations.
 * File locations should NEVER be used for READING, only for WRITING.
 * Use [[adept.repository.GitRepository]] for READ operations.
 *
 * Layout is defined as following:
 * "repos"
 *   - <repository name>: The actual repository starts here (this is the repository name)
 *     - "variants"
 *       - <id>: is the id and might be more than one sub-directory (foo/bar/zoo has 3 directory levels)
 *         - "order-<order id>": contains the order of variants. Typically there is one order per list of _compatible_ variants
 *         - <hash>: is the variant hash of the item (SHA-256 of the contents of variant.json) and is split into 2 sub directories (first 4 chars (level 1), next 4 chars (level 2), then the rest (level 3))
 *           - "variant.json": the variant metadata: attributes, requirements and artifacts references
 *           - "resolution-results.json": the exact repository information this variant requires to resolve (commit, name, variant, ..)
 *           - "info.json": extra information (home page, description, ...) not used for resolution
 *     - "locations"
 *       - "hosts.properties": hosts that are used and can be overridden in locations and uris
 *       - repositories
 *         - <repository name>: the repository name of a repository this one requires
 *           - "repository.json": contains repository locations (e.g. git uris)
 *       - artifacts
 *         - <hash>: same as variant hash, but for artifacts so this is the actual hash of the file represented by the artifact
 *           - "artifact.json": information about the hashes (file size and locations)
 */
private[adept] class Repository(val baseDir: File, val name: RepositoryName) {
  import Repository._
  require(name.value.nonEmpty, "Cannot create a repository with an empty name")

  val dir = getRepoDir(baseDir, name)

  val artifactsMetadataDir = getArtifactsMetadataDir(baseDir, name)
  val variantsMetadataDir = getVariantsMetadataDir(baseDir, name)

  private def getVariantHashDir(id: Id, hash: VariantHash) = {
    if (hash.value.size != (Level1Length + Level2Length + Level3Length))
      throw MalformedVariantHashException(this, hash)
    else {
      val level1 = new File(getIdFile(variantsMetadataDir, id), hash.value.slice(0, Level1Length))
      val level2 = new File(level1, hash.value.slice(Level2Length, Level1Length + Level2Length))
      val level3 = new File(level2, hash.value.slice(Level1Length + Level2Length, Level3Length + Level1Length + Level2Length))
      level3
    }
  }

  protected def getIdFile(rootDir: File, id: Id): File = {
    id.value.split(IdDirSep).foldLeft(rootDir) { (currentPath, dir) =>
      new File(currentPath, dir)
    }
  }

  def getVariantFile(id: Id, hash: VariantHash) = {
    ensureParentDirs(new File(getVariantHashDir(id, hash), VariantMetadataFileName))
  }

  def getResolutionResultsFile(id: Id, hash: VariantHash) = {
    ensureParentDirs(new File(getVariantHashDir(id, hash), ResolutionResultsFileName))
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

  def getOrderFile(id: Id, orderId: OrderId): File = {
    val orderDir = getIdFile(variantsMetadataDir, id)
    ensureParentDirs(new File(orderDir, OrderFileNamePrefix + orderId.value))
  }
}