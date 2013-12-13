package adept.repository

import java.io.File
import adept.core.models._
import adept.core.resolution.VariantsLoaderEngine
import adept.core.resolution.VariantsLoaderLogic
import adept.logging.Logging

class CorruptRepositoryException(val exceptionMsg: String, val id: Id, val errorMsg: String) extends Exception(exceptionMsg)

private[repository] abstract class RepositoryEngine extends VariantsLoaderEngine(VariantsLoaderLogic.default) with Logging {
  val baseDir: File

  val artifactsCacheDir = Repository.getArtifactsCacheDir(baseDir)
  val atticDir = Repository.getAtticDir(baseDir)

  def cache(file: File): File = {
    val hash = Hash.calculate(file)
    val output = getCachedArtifactFile(hash)
    if (output.isFile() && Hash.calculate(output) == hash) {
      //skip
      output
    } else {
      if (output.isFile() && Hash.calculate(output) != hash) {
        //There will be a perf hit because we are hashing files 2 times. Assuming this happens rarely (has to exist a file AND the hash is corrupt are different), so it is ok?
        val corruptHash = Hash.calculate(output)
        val atticFile = new File(atticDir, corruptHash.value)

        if ((atticDir.isDirectory || atticDir.mkdirs()) && output.renameTo(atticFile)) {
          logger.warn("Moved corrupt cached artifact " + output + " (was supposed to have hash: " + hash + " but had " + corruptHash + ") to attic: " + atticFile)
        } else {
          logger.warn("Could not store corrupt cached artifact " + output + " (was supposed to have hash: " + hash + " but had " + corruptHash + ") in attic so file is being deleted")
          output.delete()
        }
      }
      if ((output.getParentFile.isDirectory || output.getParentFile.mkdirs()) && file.renameTo(output)) output
      else throw new Exception(s"Could not create cache dir to move '$file' to '$output'")
    }
  }

  def getCachedArtifactFile(hash: Hash): File = {
    val level1Dir = hash.value.slice(0, 4)
    val level2Dir = hash.value.slice(4, 8)
    new File(new File(new File(artifactsCacheDir, level1Dir), level2Dir), hash.value)
  }
}
