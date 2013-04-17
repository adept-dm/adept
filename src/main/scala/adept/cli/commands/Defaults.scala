package adept.cli.commands

import java.io.File

object Defaults {
  def dir = new File(".adept").getAbsoluteFile()
  val name = "local"
  val conf = "default"
  val timeout = {
    import akka.util.duration._
    5.minutes
  }
}