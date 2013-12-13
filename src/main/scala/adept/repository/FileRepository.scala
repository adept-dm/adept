package adept.repository

import adept.core.models._
import java.io.File


/**
 * Read and writes to files in a repository
 */
private[adept] class FileRepository(val baseDir: File, val name: String) extends Repository {
  import Repository._
  
  // Variant
  
  def getVariantsDir(id: Id): File = {
    id.value.split(IdDirSep).foldLeft(metadataDir) { (currentPath, dir) =>
      new File(currentPath, dir)
    }
  }

  def getVariantFile(id: Id, hash: Hash): File = {
    new File(getVariantsDir(id), hash.value + "." + JsonFileEnding)
  }

  override def hasVariant(id: Id, hash: Hash): Boolean = {
    val file = getVariantFile(id, hash)
    file.isFile
  }

  override def readVariant(id: Id, hash: Hash): Either[String, Variant] = {
    import org.json4s.native.Serialization.read
    import adept.serialization.Formats._

    val file = getVariantFile(id, hash)

    usingReader(file, s"variant with id: '$id', hash: '$hash'") { fr =>
      read[Variant](fr)
    }
  }

  override def readVariants(id: Id): Either[String, Set[Variant]] = {
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

  override def writeVariant(variant: Variant): Either[String, File] = {
    import org.json4s.native.Serialization.writePretty
    import adept.serialization.Formats._

    val file = getVariantFile(variant.id, Hash.calculate(variant))
    usingWriter(file) { fw =>
      writePretty(variant, fw)
    }
  }
  
  override def deleteVariant(variant: Variant): Either[String, File] = {
    val file = getVariantFile(variant.id, Hash.calculate(variant))
    if (file.delete()) {
      //TODO: delete empty directories
      Right(file)
    } else {
      Left(s"Could not delete variant: $variant in $file")
    }
  }
  
  // Artifact Descriptor

  def getArtifactDescriptorFile(hash: Hash): File = {
    val level1Dir = hash.value.slice(0, 4)
    val level2Dir = hash.value.slice(4, 8)
    new File(new File(new File(artifactDescriptorsDir, level1Dir), level2Dir), hash.value + "." + JsonFileEnding)
  }
  
  override def hasArtifactDescriptor(hash: Hash): Boolean = {
    val file = getArtifactDescriptorFile(hash)
    file.isFile
  }

  override def readArtifactDescriptor(hash: Hash): Either[String, Artifact] = {
    import org.json4s.native.Serialization.read
    import adept.serialization.Formats._

    val file = getArtifactDescriptorFile(hash)
    usingReader(file, s"artifact for hash: $hash") { fr =>
      read[Artifact](fr)
    }
  }

  override def writeArtifactDescriptor(artifact: Artifact): Either[String, File] = {
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