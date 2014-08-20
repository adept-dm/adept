package adept.repository

import java.io.{File, FileInputStream, IOException, InputStream}

import adept.artifact.models.ArtifactHash
import adept.repository.models._
import adept.resolution.models._

case class InitException(reason: String) extends Exception("Could not initialize: " + reason)
case class MalformedVariantHashException(repo: Repository, hash: VariantHash) extends
  Exception("Variant hash: '" + hash.value + "' (size: " + hash.value.length +
    ") was not well-formed in repository: " + repo.dir.getAbsolutePath)
case class MalformedArtifactHashException(repo: Repository, hash: ArtifactHash) extends
  Exception("Artifact hash: '" + hash.value + "' was not well-formed in repository: " +
    repo.dir.getAbsolutePath)

object Repository {
  val LocationsDirName = "locations"
  val ArtifactsMetadataDirName = "artifacts"
  val RepositoryLocationsMetadataDirName = "repos"
  val VariantsMetadataDirName = "variants"
  val ReposDirName = "repos"

  val JsonFileEnding = "json"
  val ContextFileName = "context." + JsonFileEnding
  val InfoMetadataFileName = "info." + JsonFileEnding
  val VariantMetadataFileName = "variant." + JsonFileEnding
  val ArtifactMetadataFileName = "artifact." + JsonFileEnding
  val RankingFileEnding = "ranking"
  val RepositoryLocationsFileName = "repository." + JsonFileEnding

  val IdDirSep = "/" //the character in an ID that indicates a different directory

  val HashLength = 64
  val Level1Length = 4
  val Level2Length = 4
  val Level3Length = HashLength - Level1Length - Level2Length

  def getReposDir(baseDir: File) = new File(baseDir.getAbsoluteFile, ReposDirName)
  def getRepoDir(baseDir: File, name: RepositoryName) = new File(getReposDir(baseDir), name.value)

  def listRepositories(baseDir: File) = {
    val filesOrDirs = Option(getReposDir(baseDir).listFiles)
      .toSet[Array[File]]
      .flatMap(_.toSet[File])
    filesOrDirs.flatMap { fileOrDir =>
      if (fileOrDir.isDirectory) {
        Some(RepositoryName(fileOrDir.getName))
      } else {
        None
      }
    }
  }

  private def getLocationsDir(baseDir: File, name: RepositoryName) = new File(getRepoDir(baseDir, name),
    LocationsDirName)

  def getArtifactsMetadataDir(baseDir: File, name: RepositoryName) = new File(getLocationsDir(baseDir, name),
    ArtifactsMetadataDirName)
  def getRepositoryLocationsMetadataDir(baseDir: File, name: RepositoryName) = new File(getLocationsDir(
    baseDir, name), RepositoryLocationsMetadataDirName)
  def getVariantsMetadataDir(baseDir: File, name: RepositoryName) = new File(getRepoDir(baseDir, name),
    VariantsMetadataDirName)

  //merge with code in ArtifactCache (createParentDir)
  private[adept] def ensureParentDirs(file: File): File = {
    val dir = file.getParentFile
    if (!(dir.isDirectory || dir.mkdirs()))
      throw new IOException("Could not create dir: " + dir.getAbsolutePath)
    else
      file
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
 *         - "info.json" (OPTIONAL): extra information (home page, description, ...) not used for resolution
 *         - "<rank id>.ranking": contains the rank of variants (i.e defines what is the 'best' variant).
 *         Typically there is one rank file per list of _compatible_ variants
 *         - <hash>: is the variant hash of the item (SHA-256 of the contents of variant.json) and is split
 *         into 2 sub directories (first 4 chars (level 1), next 4 chars (level 2), then the rest (level 3))
 *           - "variant.json": the variant metadata: attributes, requirements and artifacts references
 *           - "resolution-results.json": the exact repository information this variant requires to resolve
 *           (commit, name, variant, ..)
 *     - "locations"
 *       TODO: - "hosts.properties" (OPTIONAL): hosts that are used and can be overridden in locations and
 *       uris TODO: this has not been implemented yet
 *       - repositories
 *         - <repository name>: the repository name of a repository this one requires
 *           - "repository.json": contains repository locations (e.g. git uris)
 *       - artifacts
 *         - <hash>: same as variant hash, but for artifacts so this is the actual hash of the file
 *         represented by the artifact
 *           - "artifact.json": information about the hashes (file size and locations)
 */
//TODO: had to remove private[adept]  but should it be there?
class Repository(val baseDir: File, val name: RepositoryName) {
  import adept.repository.Repository._
  require(name.value.nonEmpty, "Cannot create a repository with an empty name")

  val dir = getRepoDir(baseDir, name)

  val artifactsMetadataDir = getArtifactsMetadataDir(baseDir, name)
  val repositoryLocationsMetadataDir = getRepositoryLocationsMetadataDir(baseDir, name)
  val variantsMetadataDir = getVariantsMetadataDir(baseDir, name)

  def exists: Boolean = {
    dir.isDirectory
  }

  def getVariantHashDir(id: Id, hash: VariantHash) = {
    if (hash.value.size != (Level1Length + Level2Length + Level3Length))
      throw MalformedVariantHashException(this, hash)
    else {
      val level1 = new File(getIdFile(variantsMetadataDir, id), hash.value.slice(0, Level1Length))
      val level2 = new File(level1, hash.value.slice(Level2Length, Level1Length + Level2Length))
      val level3 = new File(level2, hash.value.slice(Level1Length + Level2Length, Level3Length +
        Level1Length + Level2Length))
      level3
    }
  }

  protected def getIdFile(rootDir: File, id: Id): File = {
    id.value.split(IdDirSep).foldLeft(rootDir) { (currentPath, dir) =>
      new File(currentPath, dir)
    }
  }

  def getVariantFile(id: Id, hash: VariantHash) = {
    new File(getVariantHashDir(id, hash), VariantMetadataFileName)
  }

  def ensureVariantFile(id: Id, hash: VariantHash) = {
    ensureParentDirs(getVariantFile(id, hash))
  }

  def getContextFile(id: Id, hash: VariantHash) = {
    new File(getVariantHashDir(id, hash), ContextFileName)
  }

  def ensureContextFile(id: Id, hash: VariantHash) = {
    ensureParentDirs(getContextFile(id, hash))
  }

  def getArtifactFile(hash: ArtifactHash) = {
    if (hash.value.size != (Level1Length + Level2Length + Level3Length))
      throw MalformedArtifactHashException(this, hash)
    else {
      val level1 = new File(artifactsMetadataDir, hash.value.slice(0, Level1Length))
      val level2 = new File(level1, hash.value.slice(Level2Length, Level1Length + Level2Length))
      val level3 = new File(level2, hash.value.slice(Level1Length + Level2Length, Level3Length +
        Level1Length + Level2Length))
      new File(level3, ArtifactMetadataFileName)
    }
  }

  def ensureArtifactFile(hash: ArtifactHash) = {
    ensureParentDirs(getArtifactFile(hash))
  }

  def getInfoFile(id: Id, hash: VariantHash): File = {
    val infoDir = getVariantHashDir(id, hash)
    new File(infoDir, InfoMetadataFileName)
  }
  
  def ensureInfoFile(id: Id, hash: VariantHash) = {
    ensureParentDirs(getInfoFile(id, hash))
  }

  def getRankingFile(id: Id, rankId: RankId): File = {
    val orderDir = getIdFile(variantsMetadataDir, id)
    new File(orderDir, rankId.value + "." + RankingFileEnding)
  }

  def ensureRankingFile(id: Id, rankId: RankId): File = {
    ensureParentDirs(getRankingFile(id, rankId))
  }

  def getRepositoryLocationsFile(name: RepositoryName): File = {
    val repositoryLocationsDir = new File(repositoryLocationsMetadataDir, name.value)
    new File(repositoryLocationsDir, RepositoryLocationsFileName)
  }

  def ensureRepositoryLocationsFile(name: RepositoryName): File = {
    ensureParentDirs(getRepositoryLocationsFile(name))
  }

  private[repository] def usingFileInputStream[A](file: File)(block: Either[String, Option[InputStream]] =>
    A): A = {
    if (!file.exists()) {
      block(Right(None))
    } else {
      val fis = new FileInputStream(file)
      try {
        block(Right(Some(fis)))
      } finally {
        fis.close()
      }
    }
  }
}
