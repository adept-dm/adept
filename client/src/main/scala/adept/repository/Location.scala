package adept.repository

import java.io.{File => jFile}
import java.net.URI
import akka.actor._

sealed trait Location

trait Downloadable {
  def download(dest: String, listener: ActorRef)
}

case class Torrent(file: jFile) extends Location with Downloadable {
  def download(dest: String, listener: ActorRef) = {
    throw RemoveThisNotImplementedException
  }
  override val toString = file.toURI().toString()
}

case class URL(address: String) extends Location with Downloadable {
  override val toString = new URI(address).toString()

  def download(dest: String, listener: ActorRef) = {
    throw RemoveThisNotImplementedException
  }
}

case class File(file: jFile) extends Location {
  override val toString = file.toURI().toString()
}