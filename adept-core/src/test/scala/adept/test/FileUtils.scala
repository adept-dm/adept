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
    } else throw new Exception("Could not create test dir: " + testDir.getAbsolutePath())
  }

  def usingTmpDirs[A](f: (File, File) => A) = synchronized { //<-- NOTICE
    val rootDir = new File("tmp")

    if (rootDir.isDirectory) {
      logger.debug("Deleting root tmp dir: " + rootDir.getAbsolutePath)
      import scala.reflect.io.Directory
      (new Directory(rootDir)).deleteRecursively()
    }

    val testDir1 = new File(rootDir, "test-dir-1-" + System.currentTimeMillis)
    val testDir2 = new File(rootDir, "test-dir-2-" + System.currentTimeMillis)
    val testDir1Created = testDir1.mkdirs()
    val testDir2Created = testDir2.mkdirs()
    if (testDir1Created && testDir2Created) {
      logger.debug("Created testdirs: " + testDir1.getAbsolutePath + " and " + testDir2.getAbsolutePath)
      f(testDir1, testDir2)
    } else {
      if (testDir1Created && !testDir2Created) {
        throw new Exception("Could not create test dir: " + testDir2.getAbsolutePath())
      } else if (!testDir1Created && testDir2Created) {
        throw new Exception("Could not create test dir: " + testDir1.getAbsolutePath)
      } else {
        throw new Exception("Could not create any test dir. Tried: " + testDir1.getAbsolutePath() + " and " + testDir2.getAbsolutePath())
      }
    }
  }

}