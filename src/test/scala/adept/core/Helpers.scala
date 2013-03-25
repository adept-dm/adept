package adept.core

import scala.slick.session.Database
import adept.core.db.Types._
import org.junit.rules.TemporaryFolder

object Helpers {
  import adept.core.db.DAO.driver.simple._
  import adept.core.db._
  
  def listChanges(adept: Adept): (List[ModuleRowType], List[ModuleRowType]) = {
    adept.stagedDB.withSession{ implicit s: Session =>
      Query(Modules).list.map(Modules.fromRow)
    } -> adept.mainDB.withSession{ implicit s: Session =>
      Query(Modules).list.map(Modules.fromRow)
    }
  }
  
  def tmpJar() = {
    import java.io._
    import java.math.BigInteger
    import java.security.SecureRandom
    
    val tmp = File.createTempFile("fake",".jar");
    val writer = new FileWriter(tmp)
    try { 
      writer.write( new BigInteger(130, new SecureRandom()).toString(32))
      writer.flush()
    } finally {
      tmp.deleteOnExit()
      writer.close()
    }
    tmp
  }
  
  def createTmpFolder() = {
    val f = new TemporaryFolder
    f.create()
    f.getRoot().deleteOnExit()
    f
  }
  
  def newTestAdept(newRepoName: String) = {
    val tmpFolder = createTmpFolder()
    val a = new TestAdept(tmpFolder.getRoot(), newRepoName)
    println("!!!@#@#@!@!@!@" + a.dir)
    a.testInit()
    (a -> tmpFolder)
  }
  

}