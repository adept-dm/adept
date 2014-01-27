package adept.test

import java.io.File
import adept.logging.Logging

object FileUtils extends Logging {

  def usingTempDir(f: File => Unit) = {
    val rootDir = new File("tmp")

    if (rootDir.isDirectory) {
      logger.debug("Deleting root tmp dir: " + rootDir.getAbsolutePath)
      import scala.reflect.io.Directory
      (new Directory(rootDir)).deleteRecursively()
    }

    val tmpDir = new File(rootDir, "tmp-dir-" + System.currentTimeMillis)
    if (tmpDir.mkdirs()) {
      logger.debug("Created tmp dir: " + tmpDir.getAbsolutePath)
      f(tmpDir)
    } else throw new Exception("Could not create tmp dir: " + tmpDir)
  }

}