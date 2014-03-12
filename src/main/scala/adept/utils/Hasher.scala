package adept.utils

import java.io.InputStream
import java.security.MessageDigest

object Hasher {
  private[adept] lazy val md: ThreadLocal[MessageDigest] = new ThreadLocal[MessageDigest] { //make message digest thread-"safe"
    override def initialValue() = {
      MessageDigest.getInstance("SHA-256")
    }
  }

  private def withMd[A](f: MessageDigest => A) = {
    val currentMd = md.get
    currentMd.reset()
    try {
      f(currentMd)
    } finally {
      currentMd.reset()
    }
  }

  def hash(bytes: Array[Byte]): String = withMd { currentMd =>
    currentMd.update(bytes)
    import javax.xml.bind.DatatypeConverter
    DatatypeConverter.printHexBinary(currentMd.digest()).toLowerCase
  }

  def hash(in: InputStream): String = withMd { currentMd =>
    val buf = new Array[Byte](1024 * 4) //_seemed_ to be the fastest when I tried it out when I was writing this
    var len = in.read(buf)

    //streaming was much more efficient than using digest on Array[Byte] - to be verified...
    while (len > 0) {
      currentMd.update(buf, 0, len)
      len = in.read(buf)
    }
    import javax.xml.bind.DatatypeConverter
    DatatypeConverter.printHexBinary(currentMd.digest()).toLowerCase
  }

}