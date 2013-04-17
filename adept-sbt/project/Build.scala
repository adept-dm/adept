import sbt._
import sbt.Keys._

object AdeptBuild extends Build {

  override lazy val settings = super.settings ++ Seq(
    sbtPlugin := true
  )
    
  lazy val root = Project(id = "adept-sbt",
    base = file("."),
    settings = Project.defaultSettings).dependsOn(file("../"))
}


