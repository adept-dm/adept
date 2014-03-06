package adept.artifacts

import adept.models.Artifact
import scala.concurrent.{ future, blocking, Future }
import java.io.File
import scala.concurrent.ExecutionContext
import java.net.URL
import java.nio.channels.Channels
import java.io.FileOutputStream
import adept.models.Hash

object Downloader {

  def download(locations: Set[String], hash: Hash, file: File, tmpDir: File)(implicit executionCtxt: ExecutionContext) = future {
    blocking {
      if (locations.size != 1) throw new Exception("Locations larger than 1 is not currently implemented") //TODO: implement!
      val url = new URL(locations.head)
      println("Downloading: " + url)
      val conn = url.openConnection()
      
      //TODO: this is not right, but some parts of maven repo actually requires another user-agent so I picked this random one...
      conn.setRequestProperty("User-Agent", "Mozilla/5.0 (Windows NT 5.1) AppleWebKit/535.11 (KHTML, like Gecko) Chrome/17.0.963.56 Safari/535.11")

      val rbc = Channels.newChannel(conn.getInputStream)
      val tmpFile = File.createTempFile("adept-", "-file", tmpDir)
      val fos = new FileOutputStream(tmpFile)
      fos.getChannel().transferFrom(rbc, 0, Long.MaxValue)
      println("Finished download from: " + url)
      tmpFile -> hash
    }
  }

}
