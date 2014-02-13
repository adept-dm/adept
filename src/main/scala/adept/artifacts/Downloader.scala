package adept.artifacts

import adept.models.Artifact
import scala.concurrent.{ future, blocking, Future }
import java.io.File
import scala.concurrent.ExecutionContext

object Download {

  def download(locations: Set[String], file: File, tmpDir: File)(implicit executionCtxt: ExecutionContext): Future[File] = future {
    blocking {
      ???
    }
  }
  
}
