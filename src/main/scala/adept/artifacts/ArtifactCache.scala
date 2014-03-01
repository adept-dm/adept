package adept.artifacts

import java.io.File
import adept.models.Hash
import java.io.FileInputStream
import java.io.FileOutputStream

object ArtifactCache {

  val ArtifactCacheDirName = "cache"

  private def copy(src: File, dest: File): Unit = {
    import java.io.{ File, FileInputStream, FileOutputStream }
    new FileOutputStream(dest).getChannel.transferFrom(
      new FileInputStream(src).getChannel, 0, Long.MaxValue)
  }

  def copyFromCache(baseDir: File, hash: Hash, file: File) = {
    val dir = new File(baseDir, ArtifactCacheDirName)
    if (dir.isDirectory || dir.mkdirs()) {
      val cacheFile = getCacheFile(baseDir, hash)
      if (!cacheFile.isFile) throw new Exception("Could not locate cache file: " + cacheFile.getAbsolutePath + " for hash: " + hash)
      else copy(cacheFile, file)
    } else throw new Exception("Could not create artifact directory: " + dir + " and hash: " + hash)
  }

  def getCacheFile(baseDir: File, hash: Hash) = {
    val dir = new File(baseDir, ArtifactCacheDirName)
    val cacheFile = new File(dir, hash.value)
    cacheFile
  }

  def cache(baseDir: File, file: File, expectedHash: Hash): File = {
    val dir = new File(baseDir, ArtifactCacheDirName)
    if (dir.isDirectory || dir.mkdirs()) {
      val newFile = getCacheFile(baseDir, expectedHash)
      if (newFile.isFile) {
        //TODO: verify hash?
        newFile
      } else {
        copy(file, newFile)
        newFile
      }
    } else throw new Exception("Could not create artifact directory: " + dir + " file: " + file + " hash: " + expectedHash)
  }

}