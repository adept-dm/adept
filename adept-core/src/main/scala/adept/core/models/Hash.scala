package adept.core.models

import java.io.File
import scala.util.DynamicVariable
import java.security.MessageDigest

case class Hash(value: String) {
  override def toString = value 
}

object Hash {
  private lazy val md: ThreadLocal[MessageDigest] = new ThreadLocal[MessageDigest]{ //make message digest thread-"safe"
    override def initialValue() = {
      MessageDigest.getInstance("SHA-1")
    }
  }
  
  private def encode(bytes: Array[Byte]) = {
    md.get().digest(bytes).map(b => "%02X" format b).mkString.toLowerCase
  }
  
  def mix(hashes: Seq[Hash]): Hash = {
    val mixString = hashes.map(_.value).mkString(",")
    Hash(encode(mixString.getBytes))
  } 
  
  def calculate(hashes: Seq[Hash]): Hash = {
    hashes.par.foldLeft(Hash("")){ case (current, next) =>
      mix(Seq(current, next))
    }
  }
  
  def calculate(string: String): Hash = {
    Hash(encode(string.getBytes))
  }
  
  private def streamingEncode(file: File): String = {
    import java.io._
    val in = new FileInputStream(file)
    val currentMd = md.get()
    currentMd.reset()
    val buf = new Array[Byte](1024 * 4) //_seemed_ to be the fastest when I tried it out when I was writing this
    try {
      var len = in.read(buf) 
  
      while (len > 0) { 
        currentMd.update(buf, 0, len) 
        len = in.read(buf) 
      }
      currentMd.digest().map(b => "%02X" format b).mkString.toLowerCase
    } finally {
      currentMd.reset()
      in.close 
    }
  }
  
  def calculate(file: File): Hash = {
    Hash(streamingEncode(file))
  }
}
