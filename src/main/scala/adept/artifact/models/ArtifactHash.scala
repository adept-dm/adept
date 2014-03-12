package adept.artifact.models

import java.io.File
import java.io.FileInputStream
import adept.utils.Hasher

case class ArtifactHash(value: String) extends AnyVal

object AritfactHash {
  def fromFile(file: File): ArtifactHash = {
    val fos = new FileInputStream(file)
    try {
      ArtifactHash(Hasher.hash(fos))
    } finally {
      fos.close()
    }
  }
}