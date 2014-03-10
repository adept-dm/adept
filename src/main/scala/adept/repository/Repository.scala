package adept.repository

import java.io.File
import adept.models._
import org.eclipse.jgit.lib.Constants

case class InitException(repo: Repository, reason: String) extends Exception("Could not initialize '" + repo.dir.getAbsolutePath + "': " + reason)

object Repository {

  //avoid cache collisions with hashes which are 64 chars
  //  val MaxIdLength = 63 //TODO: use this?

  val ArtifactsMetadataDirName = "artifacts"
  val VariantsMetadataDirName = "variants"
  val RepositoryMetadataDirName = "repos"
  val UniverseMetadataDirName = "universe"

  val UniverseOrderFileName = "order"
  val ReposDirName = "repos"
  val JsonFileEnding = "json"

  val DefaultBranchName = "master"
  val Head = Constants.HEAD

  val GitPathSep = "/" //the character that separates paths in Git
  val IdDirSep = "/" //the character in an ID that indicates a different directory

  def getReposDir(baseDir: File) = new File(baseDir, ReposDirName)
  def getRepoDir(baseDir: File, name: String) = new File(getReposDir(baseDir), name)
  def getArtifactsMetadataDir(baseDir: File, name: String) = new File(getRepoDir(baseDir, name), ArtifactsMetadataDirName)
  def getRepositoryMetadataDir(baseDir: File, name: String) = new File(getRepoDir(baseDir, name), RepositoryMetadataDirName)
  def getVariantsMetadataDir(baseDir: File, name: String) = new File(getRepoDir(baseDir, name), VariantsMetadataDirName)
  def getUniverseOrderFile(baseDir: File, name: String) = {
    val dir = new File(getRepoDir(baseDir, name), UniverseMetadataDirName)
    new File(dir, UniverseOrderFileName)
  }
}

class Repository(baseDir: File, name: String) {
  import Repository._

  val dir = getRepoDir(baseDir, name)

  def getIdDir(rootDir: File, id: Id): File = {
    id.value.split(IdDirSep).foldLeft(rootDir) { (currentPath, dir) =>
      new File(currentPath, dir)
    }
  }

  def getVariantsMetadataDir(id: Id): File = {
    getIdDir(Repository.getVariantsMetadataDir(baseDir, name), id)
  }

  def getVariantsMetadataFile(id: Id, hash: Hash): File = {
    new File(getVariantsMetadataDir(id), hash.value + "." + JsonFileEnding)
  }

  def getArtifactMetadataFile(hash: Hash): File = {
    new File(getArtifactsMetadataDir(baseDir, name), hash.value + "." + JsonFileEnding)
  }

  def getRepositoryMetadataDir(id: Id): File = {
    getIdDir(Repository.getRepositoryMetadataDir(baseDir, name), id)
  }

  def getRepositoryMetadataFile(id: Id, hashes: Set[Hash]): File = {
    new File(getRepositoryMetadataDir(id), Hash.calculate(hashes.mkString) + "." + JsonFileEnding)
  }

  def getUniverseOrderFile(): File = {
    Repository.getUniverseOrderFile(baseDir, name)
  }

  //----
  
  def createParentDir(file: File): File = {
    val dir = file.getParentFile
    if (!(dir.isDirectory() || dir.mkdirs())) throw InitException(this, "Could not create repository metadata dir: " + dir.getAbsolutePath)
    else dir
  }

  //----
  
  
}