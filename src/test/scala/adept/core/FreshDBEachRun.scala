package adept.core

import java.sql.Connection
import org.scalatest._
import java.io.File
import org.junit.rules.TemporaryFolder
import adept.core.db.DAO.driver.simple._

class TestAdept(dir: File, repo: Repository) extends Adept(dir, repo) {
  def testInit() = {
    init()
  }
}

trait FreshDBEachRun extends FunSuite with BeforeAndAfterEach {
  import Helpers._
  import TestData._

  var tmpFolder: TemporaryFolder = null
  var adept: TestAdept = null
  
  override def beforeEach = {
    newTestAdept(repoName) match {
      case (a, t) => {
        adept = a
        tmpFolder = t 
      } 
    }
  }
  
  override def afterEach = {
    tmpFolder.delete()
  }
}