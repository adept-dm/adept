package adept.repository

import adept.core.models._
import java.io._

object Repository {
  val ArtifactDescriptorDirName = "artifacts"
  val ArtifactCacheDirName = "cache"
  val MetadataDirName = "metadata"
  val ReposDirName = "repos"
  val JsonFileEnding = "json"

  private def repoDir(baseDir: File, name: String) = new File(new File(baseDir, ReposDirName), name)
  def artifactDescriptorsDir(baseDir: File, name: String) = new File(repoDir(baseDir, name), ArtifactDescriptorDirName)
  def artifactsCacheDir(baseDir: File) = new File(baseDir, ArtifactCacheDirName)
  def metadataDir(baseDir: File, name: String) =  new File(repoDir(baseDir, name), MetadataDirName)
}

case class Repository(val baseDir: File, val name: String) {

  import Repository._
  val artifactDescriptorsDir = Repository.artifactDescriptorsDir(baseDir, name)
  val metadataDir = Repository.metadataDir(baseDir, name)

  private def usingReadLock[A](f: => A) = { //TODO: implement using Actor (ideally can use any file writer/reader as long as no other actor/process touches this file?)
    f
  }

  private def usingWriteLock[A](f: => A) = { //TODO: implement using Actor (ideally can use any file writer/reader as long as no other actor/process touches this file?)
    f
  }

  def getVariantsDir(id: Id): File = {
    id.value.split("/").foldLeft(metadataDir) { (currentPath, dir) =>
      new File(currentPath, dir)
    }
  }

  def getVariantFile(id: Id, hash: Hash): File = {
    new File(getVariantsDir(id), hash.value + "." + JsonFileEnding)
  }

  private def usingReader[A](file: File, info: String)(f: FileReader => A): Either[String, A] = usingReadLock {
    if (file.isFile && file.canRead) {
      val fr = new FileReader(file)
      try {
        Right(f(fr))
      } finally {
        fr.close()
      }
    } else { Left(s"Cannot read ($info) from file '$file': { is file: ${file.isFile}, can read: ${file.canRead} }") }
  }

  def hasVariant(id: Id, hash: Hash): Boolean = {
    val file = getVariantFile(id, hash)
    file.isFile
  }

  def readVariant(id: Id, hash: Hash): Either[String, Variant] = {
    import org.json4s.native.Serialization.read
    import adept.serialization.Formats._

    val file = getVariantFile(id, hash)

    usingReader(file, s"variant with id: '$id', hash: '$hash'") { fr =>
      read[Variant](fr)
    }
  }

  def readVariants(id: Id): Either[String, Set[Variant]] = {
    import org.json4s.native.Serialization.read
    import adept.serialization.Formats._

    val dir = getVariantsDir(id)
    val jsonFiles = if (dir != null && dir.isDirectory)
      dir.listFiles.filter { file =>
        file != null && file.getName.endsWith(JsonFileEnding)
      }.toSet
    else Set.empty

    //imperative style makes this easier to read:
    var result: Either[String, Set[Variant]] = Right(Set.empty)

    for (file <- jsonFiles) {
      var i = 0

      val maybeVariant = usingReader(file, s"multiple variants with id: '$id'") { fr =>
        read[Variant](fr)
      }

      (result, maybeVariant) match {
        case (Right(variants), Right(variant)) => result = Right(variants + variant)
        case (Left(errorMsg), Right(variant)) => result = Left(errorMsg + s"; $file was ok")
        case (Right(variants), Left(errorMsg)) => result = Left(errorMsg)
        case (Left(errorMsg1), Left(errorMsg2)) => result = Left(errorMsg1 + "; " + errorMsg2)
      }
    }

    result
  }

  private def usingWriter(file: File)(f: FileWriter => Unit): Either[String, File] = usingWriteLock {
    if ((file.getParentFile.isDirectory || file.getParentFile.mkdirs()) && file.getParentFile.canWrite) {
      val fw = new FileWriter(file)
      try {
        f(fw)
        fw.flush()
        Right(file)
      } finally {
        fw.close()
      }
    } else Left(s"Cannot write '$file' to  parent directory '${file.getParentFile}': { can read: ${file.getParentFile.canWrite}, is dir: ${file.getParentFile.isDirectory} }")
  }

  def writeVariant(variant: Variant): Either[String, File] = {
    import org.json4s.native.Serialization.writePretty
    import adept.serialization.Formats._

    val file = getVariantFile(variant.id, Hash.calculate(variant))
    usingWriter(file) { fw =>
      writePretty(variant, fw)
    }
  }

  def hasArtifactDescriptor(hash: Hash): Boolean = {
    val file = getArtifactDescriptorFile(hash)
    file.isFile
  }

  def readArtifactDescriptor(hash: Hash): Either[String, Artifact] = {
    import org.json4s.native.Serialization.read
    import adept.serialization.Formats._

    val file = getArtifactDescriptorFile(hash)
    usingReader(file, s"artifact for hash: $hash") { fr =>
      read[Artifact](fr)
    }
  }

  def getArtifactDescriptorFile(hash: Hash): File = {
    val level1Dir = hash.value.slice(0, 4)
    val level2Dir = hash.value.slice(4, 8)
    new File(new File(new File(artifactDescriptorsDir, level1Dir), level2Dir), hash.value + "." + JsonFileEnding)
  }

  def writeArtifactDescriptor(artifact: Artifact): Either[String, File] = {
    import org.json4s.native.Serialization.writePretty
    import adept.serialization.Formats._

    val file = getArtifactDescriptorFile(artifact.hash)
    if (file.isFile) {
      readArtifactDescriptor(artifact.hash) match {
        case Right(oldArtifact) =>
          if (oldArtifact.hash == artifact.hash && oldArtifact.size == artifact.size) {
            usingWriter(file) { fw =>
              writePretty(artifact.copy(locations = artifact.locations ++ oldArtifact.locations), fw)
            }
          } else Left(s"Cannot write and merge new with old artifact! Found old file for ${artifact.hash}: '$file', but it does not have expected hash and size: {size: ${oldArtifact.size}, expected size: ${artifact.size}; hash: ${oldArtifact.hash}, expected hash: ${artifact.hash} }")
        case Left(errorMsg) => Left(s"Cannot write and merge new artifact: found previous file for ${artifact.hash}: '$file', but could not read it. Error: $errorMsg")
      }
    } else {
      usingWriter(file) { fw =>
        writePretty(artifact, fw)
      }
    }
  }

}