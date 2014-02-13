package adept.artifacts

import java.io.File
import adept.models.Hash
import java.io.FileInputStream
import java.io.FileOutputStream

object ArtifactCache {

  val ArtifactCacheDirName = "cache"

  private def copy(src: File, dest: File) {
    import java.io.{ File, FileInputStream, FileOutputStream }
    new FileOutputStream(dest).getChannel.transferFrom(
      new FileInputStream(src).getChannel, 0, Long.MaxValue)
  }

  def getArtifactCacheFile(baseDir: File, hash: Hash) = {
    val dir = new File(baseDir, ArtifactCacheDirName)
    if (dir.isDirectory || dir.mkdirs()) {
      new File(dir, hash.value)
    } else throw new Exception("Could not create artifact directory: " + dir + " and hash: " + hash)
  }

  def cache(baseDir: File, file: File, expectedHash: Hash): File = {
    val dir = new File(baseDir, ArtifactCacheDirName)
    val newFile = getArtifactCacheFile(baseDir, expectedHash)
    if (newFile.isFile) {
      //TODO: verify hash?
      newFile
    } else {
      copy(file, newFile)
      newFile
    }
  }

}