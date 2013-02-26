package adept.core

import java.sql.Connection
import org.scalatest._


trait FreshDBEachRun extends FunSpec with BeforeAndAfterEach {
  import db.driver.simple._
  import Helpers._
  import TestData._

  //FIXME: hack to leave in memory database running without having a session
  var connection: Connection = null
  
  implicit lazy val database = Database.forURL("jdbc:h2:mem:adept-test-"+this.hashCode(), driver = "org.h2.Driver") 
  
  
  override def beforeEach = {
    connection = database.createSession.conn
    Adept.init(repo)
  }
  
  override def afterEach = {
    database.withSession{ 
import Database.threadLocalSession
      db.allDDLs.drop
    }
    connection = null
  }
}