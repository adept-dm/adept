package adept.artifact

import scala.concurrent.{ future, blocking, Future }
import java.io.File
import scala.concurrent.ExecutionContext
import java.net.URL
import java.nio.channels.Channels
import java.io.FileOutputStream
import adept.artifact.models._

class Downloader(tmpDir: File) {
  def download(artifact: Artifact)(implicit executionCtxt: ExecutionContext) = future {
    blocking {
      if (artifact.locations.size != 1) throw new Exception("Locations different from 1 is not currently implemented and we got: " + artifact) //TODO: implement!
      val url = new URL(artifact.locations.head)
      val conn = url.openConnection()
      
      //TODO: this is not right, but some parts of maven repo actually requires another user-agent so I picked this random one... I should chose a new one
      conn.setRequestProperty("User-Agent", "Mozilla/5.0 (Windows NT 5.1) AppleWebKit/535.11 (KHTML, like Gecko) Chrome/17.0.963.56 Safari/535.11")

      val rbc = Channels.newChannel(conn.getInputStream)
      val tmpFile = File.createTempFile("adept-", "-file", tmpDir)
      val fos = new FileOutputStream(tmpFile)
      fos.getChannel().transferFrom(rbc, 0, Long.MaxValue)
      artifact -> tmpFile
    }
  }

}
