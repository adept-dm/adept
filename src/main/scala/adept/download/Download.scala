package adept.download

import adept.models._
import java.io.File
import akka.actor._
import akka.util.duration._
import akka.util.Timeout
import akka.dispatch.Future
import akka.dispatch.ExecutionContext
import akka.dispatch.Await
import akka.util.Timeout
import java.net.URL
import org.slf4j.LoggerFactory
import akka.util._

private[adept] object Download  {
  private val logger = LoggerFactory.getLogger(this.getClass) 
  
  def apply(downloadbles: Seq[(Hash, Coordinates, Set[String], File)], timeout: FiniteDuration): Seq[Either[String, File]] = {
    if (downloadbles.nonEmpty) {
      logger.trace("downloading "+downloadbles.size+" modules...")
      
      val system = ActorSystem("adept-download")
      implicit val executionContext = akka.dispatch.ExecutionContext.defaultExecutionContext(system)
      val progressIndicator = system.actorOf(Props[ProgressIndicator])
      try {
        val perhapsFiles = (downloadbles.map{ case (hash, coords, locations, file) =>
          val tmpFiles = locations.map{ location =>
            location -> File.createTempFile(coords.org+"-"+coords.name+"-"+coords.version, ".jar")
          }
          
          val possibleJars = tmpFiles.map{ case (url, file) => 
            logger.trace("temp file from "+url+" to "+file)
            progressIndicator ! Started
            Client.download(new URL(url), file, progressIndicator)(timeout, system) //TODO: now we start download from all sources. we should have a smarter way to do this
          }
          
          Future.find(possibleJars)(_.isRight)
            .map{ maybe =>
              maybe.map(_.right.get)  -> (hash, coords, locations, file, tmpFiles)//.get should be successful because of isRight
            }
        })
  
        
        logger.trace("waiting "+timeout+" for downloads to complete... ")
        val maybeFiles = Await.result(Future.sequence(perhapsFiles), timeout)
        maybeFiles.map{ case (maybeFile, (hash, coords, locations, jarFile, tmpFiles)) =>
          maybeFile.map{ file =>
           val artifactHash = Hash.calculate(file)
            if (hash == artifactHash) {
              logger.trace("renaming temp file from "+file+" to "+jarFile)
              if (file.renameTo(jarFile)) {
                val deleteFiles = tmpFiles.filter{ case (_, tmpFile) => tmpFile != file }
                logger.trace("cleaning up temp files: "+{deleteFiles.mkString(",")})
                deleteFiles.foreach{ case (_, tmpFile) => tmpFile.delete() }
                Right(jarFile)
              } else {
                Left("could not rename temp file from "+file+" to "+jarFile)
              }
            } else {
              Left("expected temporary file downloaded for "+coords+" to have hash: "+ hash+" but got "+artifactHash)
            }
          }.getOrElse{
            Left("could not download artifacts for "+coords+" from: "+ locations.mkString(","))
          }
        }
      } finally {
        system.shutdown
      }
    } else {
      logger.trace("no modules to download...")
      Seq.empty
    }
  }
}