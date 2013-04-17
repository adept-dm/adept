package adept.models

import java.io.File

case class Artifact(hash: Hash, locations: Set[String])

object Artifact{
  def fromFile(file: File, locations: Set[String]) = {
    Artifact(Hash.calculate(file), locations)
  }
}