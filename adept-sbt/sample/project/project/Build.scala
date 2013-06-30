import sbt._
import sbt.Keys._

object AdeptBuild extends Build {

  override lazy val settings = super.settings ++ Seq(
  )

  lazy val adeptSbt = ProjectRef(file("../../.."), "adept-sbt")
    
  lazy val root = Project(id = "adept-sbt-sample-project",
    base = file("."),
    settings = Project.defaultSettings).dependsOn(adeptSbt)  
}


