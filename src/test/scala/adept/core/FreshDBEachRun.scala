package adept.core

import java.sql.Connection
import org.scalatest._
import java.io.File
import org.junit.rules.TemporaryFolder
import adept.core.db.DAO.driver.simple._
  
trait FreshDBEachRun extends FunSuite with BeforeAndAfterEach {
  import Helpers._
  import TestData._

  var tmpFolder = {
    val f = new TemporaryFolder
    f.create()
    f.getRoot().deleteOnExit()
    f
  }
  var adept: TestAdept = null
  
  class TestAdept(dir: File) extends Adept(dir) {
    def testInit() = init()
    val mainDBpublic = mainDB
    val stagedDBpublic = stagedDB
  }
  
  override def beforeEach = {
    adept = new TestAdept(tmpFolder.getRoot())
    adept.testInit()
  }
  
  override def afterEach = {
    tmpFolder.delete()
  }
}