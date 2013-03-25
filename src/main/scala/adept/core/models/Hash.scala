package adept.core.models

import java.io.File

case class Hash(value: String) {
  override def toString = value 
}

object Hash {
  private lazy val md = java.security.MessageDigest.getInstance("SHA-1")
  private def encode(bytes: Array[Byte]) = {
    md.digest(bytes).map(b => "%02X" format b).mkString.toLowerCase
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
  
  private def readBytes(file: File): Array[Byte] = {
    import java.io._
    val in = new FileInputStream(file)
    var out = new ByteArrayOutputStream() 
    val buf = new Array[Byte](4096)
    try {
      var len = in.read(buf) 
  
      while (len > 0) { 
        out.write(buf, 0, len) 
        len = in.read(buf) 
      }
      out.toByteArray
    } finally {
      out.flush 
      out.close 
      in.close 
    }
  }
  
  def calculate(file: File): Hash = {
    val hashString = encode(readBytes(file))
    Hash(hashString)
  }
}