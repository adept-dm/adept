package adept.sbt

import sbt._

object AdeptKeys {
  val adeptDirectory = settingKey[File]("Adept's home directory. Defaults to: ~/.adept")
}