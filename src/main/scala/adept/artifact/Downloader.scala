package adept.artifact

import scala.concurrent.{ future, blocking, Future }
import java.io.File
import scala.concurrent.ExecutionContext
import java.net.URL
import java.nio.channels.Channels
import java.io.FileOutputStream
import adept.artifact.models._
import java.io.IOException
import adept.logging.Logging

case class ArtifactDownloadException(artifact: Artifact, exception: Exception) extends Exception

class Downloader(tmpDir: File) extends Logging {
  val userAgent = "AdeptDownloader/1.0-ALPHA"
  val defaultMaxRetries = 5
  
  def download(artifact: Artifact, maxRetries: Int = defaultMaxRetries)(implicit executionCtxt: ExecutionContext) = future {
    blocking {
      if (artifact.locations.size != 1) throw new Exception("Locations different from 1 is not currently implemented and we got: " + artifact) //TODO: implement!
      val url = new URL(artifact.locations.head)
      var result: Option[(Artifact, File)] = None
      var retries = 0
      while (!result.isDefined) {
        try {
          val tmpFile = File.createTempFile("adept-", "-file", tmpDir)

          if (retries > 0) logger.debug("Retrying....")

          val conn = url.openConnection()
          conn.setRequestProperty("User-Agent", userAgent)
          val rbc = Channels.newChannel(conn.getInputStream)
          val fos = new FileOutputStream(tmpFile)
          fos.getChannel().transferFrom(rbc, 0, Long.MaxValue)

          result = Some(artifact -> tmpFile)
        } catch {
          case ioException: IOException =>
            logger.debug("Got exception: "  + ioException.getMessage() + " cause: " + ioException.getCause())
            retries = retries + 1
            val last = System.currentTimeMillis()
            logger.debug("Sleeping before retrying...")
            Thread.sleep(500)
            if (retries > maxRetries) throw ArtifactDownloadException(artifact, ioException)
        }
      }
      result.getOrElse {
        throw new ArtifactDownloadException(artifact, new Exception("Could not download for an unknown reason: " + artifact))
      }
    }
  }

}
