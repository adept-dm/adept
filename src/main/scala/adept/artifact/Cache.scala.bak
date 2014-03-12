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

  def createParentDir(file: File) = {
    val dir = file.getParentFile()
    if (dir.isDirectory || dir.mkdirs()) {
      dir
    } else throw new Exception("Could not create parent directory: " + dir)
  }

  def cacheFile(baseDir: File, hash: Hash, filename: String): File = {
    val artifactDir = new File(baseDir, ArtifactCacheDirName)
    assert(hash.value.size > 8) //we are slicing later and need at least 8 chars
    val level1 = new File(artifactDir, hash.value.slice(0, 4))
    val level2 = new File(level1, hash.value.slice(4, 8))
    val level3 = new File(level2, hash.value)
    new File(level3, filename)
  }

  /** Get current cache file if exists. Creates a new one if there are other cached files with the same hash, but different file names */
  def getOrCreateExistingCacheFile(baseDir: File, hash: Hash, filename: String): Option[File] = {
    val currentCacheFile = cacheFile(baseDir, hash, filename)

    if (currentCacheFile.isFile) Some(currentCacheFile)
    else {
      val parentDir = createParentDir(currentCacheFile)
      val foundFile = parentDir.listFiles().find { f =>
        Hash.calculate(f) == hash
      }
      foundFile.map { f =>
        copy(f, currentCacheFile)
        currentCacheFile
      }
    }
  }

  def cache(baseDir: File, file: File, expectedHash: Hash, filename: String): File = {
    getOrCreateExistingCacheFile(baseDir, expectedHash, filename).getOrElse {
      val newCacheFile = cacheFile(baseDir, expectedHash, filename)
      copy(file, newCacheFile)
      newCacheFile
    }
  }

}