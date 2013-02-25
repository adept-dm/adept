package adept.api

import scala.slick.session.Database


object Helpers {
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
  
  def getDatabase(id: String) = {
    Database.forURL("jdbc:h2:mem:adept-test-"+id, driver = "org.h2.Driver") 
  }
}