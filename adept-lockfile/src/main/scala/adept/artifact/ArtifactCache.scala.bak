package adept.artifact

import java.io.File
import java.io.FileInputStream
import java.io.FileOutputStream
import adept.artifact.models.ArtifactHash
import adept.utils.Hasher

object ArtifactCache {

  val ArtifactCacheDirName = "cache"

  private def copy(src: File, dest: File): Unit = {
    import java.io.{ File, FileInputStream, FileOutputStream }
    val fos = new FileOutputStream(dest)
    val fis = new FileInputStream(src)
    try {
      fos.getChannel.transferFrom(
        fis.getChannel, 0, Long.MaxValue)
    } finally {
      fos.close()
      fis.close()
    }
  }

  private def createParentDir(file: File) = {
    import adept.repository.Repository.ensureParentDirs
    ensureParentDirs(file)
    val dir = file.getParentFile()
    dir
  }

  def getCacheFile(baseDir: File, hash: ArtifactHash, filename: String): File = {
    import adept.repository.Repository.{ Level1Length, Level2Length, Level3Length, HashLength }
    val artifactDir = new File(baseDir, ArtifactCacheDirName)
    assert(hash.value.size == HashLength) //we are slicing later and need at least 8 chars
    val level1 = new File(artifactDir, hash.value.slice(0, Level1Length))
    val level2 = new File(level1, hash.value.slice(Level1Length, Level1Length + Level2Length))
    val level3 = new File(level2, hash.value.slice(Level1Length + Level2Length, HashLength))
    new File(level3, filename)
  }

  private def hashFile(file: File) = {
    val fis = new FileInputStream(file)
    try {
      Hasher.hash(fis)
    } finally {
      fis.close()
    }
  }

  /** Get current cache file if exists. Creates a new one if there are other cached files with the same hash, but different file names */
  def getOrCreateExistingCacheFile(baseDir: File, hash: ArtifactHash, filename: String, verify: Boolean = true): Option[File] = {
    val currentCacheFile = getCacheFile(baseDir, hash, filename)

    if (currentCacheFile.isFile) {
      if (verify) assert(hashFile(currentCacheFile) == hash.value, "Expected " + filename + " to have hash " + hash + " but file: " + currentCacheFile.getAbsolutePath() + " has a different hash!")
      Some(currentCacheFile)
    } else {
      val parentDir = createParentDir(currentCacheFile)
      val foundFile = parentDir.listFiles().find { f =>
        !verify || hashFile(f) == hash.value
      }
      foundFile.map { f =>
        copy(f, currentCacheFile)
        currentCacheFile
      }
    }
  }

  def cache(baseDir: File, srcFile: File, expectedHash: ArtifactHash, filename: String): File = {
    val actualHash = hashFile(srcFile)
    if (actualHash != expectedHash.value) throw new Exception("Expected file: " + srcFile.getAbsolutePath + " (with new name: " + filename + ") to have hash: " + expectedHash.value + " but it was: " + actualHash)
    else
      getOrCreateExistingCacheFile(baseDir, expectedHash, filename).getOrElse {
        val newCacheFile = getCacheFile(baseDir, expectedHash, filename)
        copy(srcFile, newCacheFile)
        newCacheFile
      }
  }

}