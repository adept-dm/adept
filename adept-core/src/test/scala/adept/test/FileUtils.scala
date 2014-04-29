package adept.test

import java.io.File
import adept.logging.Logging

object FileUtils extends Logging {

  def usingTmpDir[A](f: File => A) = synchronized { //<-- NOTICE
    val rootDir = new File("tmp")

    if (rootDir.isDirectory) {
      logger.debug("Deleting root tmp dir: " + rootDir.getAbsolutePath)
      import scala.reflect.io.Directory
      (new Directory(rootDir)).deleteRecursively()
    }

    val testDir = new File(rootDir, "test-dir-" + System.currentTimeMillis)
    if (testDir.mkdirs()) {
      logger.debug("Created testdir: " + testDir.getAbsolutePath)
      f(testDir)
    } else throw new Exception("Could not create test dir: " + testDir)
  }

}