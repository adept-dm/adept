package adept.remote

import adept.core._
import util._
import java.io.File
import scala.slick.session.Database
import scala.collection.parallel.ParSeq
import com.ning.compress.lzf.util.LZFFileInputStream

class Client(dir: File) {
  
  private def download(url: String): File = {
    null 
  }
  
  private def databaseParts(url: String): Seq[String] = {
    null 
  }
  
  def unzip(file: File): File = null
  
  def createAdeptDir(): File = null
  def moveToAdeptDir(adeptDir: File, files: ParSeq[File]): File = null
  
  def databaseFor(dir: File): Database = null
  
  def clone(url: String): Try[Repository]= {
    val downloadedFiles = databaseParts(url).par.map{ file => 
      download(file)
    }
    val unzippedFiles = downloadedFiles.map(f => unzip(f))
    val adeptDir = createAdeptDir()
    Adept.repoList(databaseFor(moveToAdeptDir(adeptDir, unzippedFiles)))
    null
  }
  
  def repoUrlFor(repoName: String): Try[String] = {
    null
  }
  
  def readChangeSets(file: File): Seq[ChangeSet] = {
    val is = new LZFFileInputStream(file)
    try {
      val lines = io.Source.fromInputStream(is).getLines.mkString("\n")
      //...
    } finally{
      is.close
    }
    
    null
  }
  
  def pull(repoName: String)(implicit database: Database): Try[Repository] = {
    
    for {
      url <- repoUrlFor(repoName)
    } yield {
      val zipFiles = databaseParts(url).par.map{ file => 
        download(file)
      }
      
      val changeSets = zipFiles.par.flatMap{f => 
        
        readChangeSets(f)
      }
      Adept.merge(changeSets)(database)
    }
   
     //Adept.merge(repoName, changeSet)

    
    null
  }
}