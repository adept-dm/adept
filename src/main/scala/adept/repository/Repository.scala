package adept.repository

import adept.core.models._
import java.io._

object Repository {

  //avoid cache collisions with hashes which are 64 chars
  val MaxIdLength = 63

  val ArtifactDescriptorDirName = "artifacts"
  val ArtifactCacheDirName = "cache"
  val MetadataDirName = "metadata"
  val ReposDirName = "repos"
  val JsonFileEnding = "json"
  val AtticDir = "attic"
  val InitTag = "init"

  val IdDirSep = "/" //the character in an ID that indicates a different directory

  def getReposDir(baseDir: File) = new File(baseDir, ReposDirName)
  def getArtifactsCacheDir(baseDir: File) = new File(baseDir, ArtifactCacheDirName)
  def getAtticDir(baseDir: File) = new File(baseDir, AtticDir)
  def getRepoDir(baseDir: File, name: String) = new File(getReposDir(baseDir), name)
  def getArtifactDescriptorsDir(baseDir: File, name: String) = new File(getRepoDir(baseDir, name), ArtifactDescriptorDirName)
  def getMetadataDir(baseDir: File, name: String) = new File(getRepoDir(baseDir, name), MetadataDirName)
}

//FIXME:
//TODO: WE HAVE TO FIX THIS TYPE HIEARCHI THERE IS SOMETHING STRANGE WITH IT?
private[repository] trait RepositoryBase {
  import Repository._

  val baseDir: File
  val name: String
  val repoDir = getRepoDir(baseDir, name)
  val artifactDescriptorsDir = getArtifactDescriptorsDir(baseDir, name)
  val metadataDir = getMetadataDir(baseDir, name)
}

private[repository] trait Repository extends RepositoryBase {

  private def usingReadLock[A](f: => A) = { //TODO: implement using Actor (ideally can use any file writer/reader as long as no other actor/process touches this file?)
    f
  }

  private def usingWriteLock[A](f: => A) = { //TODO: implement using Actor (ideally can use any file writer/reader as long as no other actor/process touches this file?)
    f
  }

  private[repository] def usingReader[A](file: File, info: String)(f: FileReader => A): Either[String, A] = usingReadLock {
    if (file.isFile && file.canRead) {
      val fr = new FileReader(file)
      try {
        Right(f(fr))
      } finally {
        fr.close()
      }
    } else { Left(s"Cannot read ($info) from file '$file': { is file: ${file.isFile}, can read: ${file.canRead} }") }
  }

  private[repository] def usingWriter(file: File)(f: FileWriter => Unit): Either[String, File] = usingWriteLock {
    if ((file.getParentFile.isDirectory || file.getParentFile.mkdirs()) && file.getParentFile.canWrite) {
      val fw = new FileWriter(file.getAbsoluteFile)
      try {
        f(fw)
        fw.flush()
        Right(file)
      } finally {
        fw.close()
      }
    } else Left(s"Cannot write '${file.getAbsoluteFile}' to  parent directory '${file.getParentFile}': { can write: ${file.getParentFile.canWrite}, is dir: ${file.getParentFile.isDirectory} }")
  }

  /// Variant
  def hasVariant(id: Id, hash: Hash): Boolean

  def readVariant(id: Id, hash: Hash): Either[String, Variant]

  def readVariants(id: Id): Either[String, Set[Variant]]

  def writeVariant(variant: Variant): Either[String, File]

  def deleteVariant(variant: Variant): Either[String, File]

  /// Artifact Descriptor

  def hasArtifactDescriptor(hash: Hash): Boolean

  def readArtifactDescriptor(hash: Hash): Either[String, Artifact]

  def writeArtifactDescriptor(artifact: Artifact): Either[String, File]

}