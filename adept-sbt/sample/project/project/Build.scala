import sbt._
import sbt.Keys._

object AdeptBuild extends Build {

  override lazy val settings = super.settings ++ Seq(
  )
    
  lazy val root = Project(id = "adept-sbt-sample-1-project",
    base = file("."),
    settings = Project.defaultSettings).dependsOn(file("../.."))
  
}


