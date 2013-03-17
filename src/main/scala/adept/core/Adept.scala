package adept.core

import java.io.File
import collection.parallel.immutable.ParSeq

import adept.core.models._
import adept.core.operations._
import adept.core.remote._
import util._

object Adept {
  def apply(dir: File): Adept = {
    new Adept(dir)
  }
  def init(dir: File): Try[Adept] = {
    if (dir.exists)
     Failure(new Exception(s"cannot init adept in $dir because it exists already"))
    else 
     Success((new Adept(dir)).init())
  }
}

class Adept protected(dir: File) {
  import adept.core.db.DAO.driver.simple._
  
  protected def database(prefixFile: File) = {
    //21 == 2**21 bytes == 2 Mb
    Database.forURL("jdbc:h2:split:21:"+ prefixFile.toURI, driver = "org.h2.Driver") 
  } 
    
  protected lazy val mainDB = database(new File(dir, "main"))
  protected lazy val stagedDB = database(new File(dir, "staged"))
  
  protected def init(): Adept = {
    mainDB.withSession{s: Session => adept.core.db.DAO.allDDLs.create(s) }
    stagedDB.withSession{s: Session => adept.core.db.DAO.allDDLs.create(s) }
    this
  }
  
  def commit(): Try[Hash] = {
    Commit(stagedDB, mainDB)
  }
  
  def delete(hash: Hash): Try[Hash] = {
    Delete(hash, stagedDB, mainDB)
  }
  
  def set(module: Module): Try[Hash] = {
    Set(module, stagedDB, mainDB)
  }
  
  def server() = {
    val adeptServer = new Server(mainDB, 1337)
    adeptServer.start
  }
  
}
