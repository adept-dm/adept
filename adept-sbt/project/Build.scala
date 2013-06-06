import sbt._
import sbt.Keys._

object AdeptBuild extends Build {

  override lazy val settings = super.settings ++ Seq(
    sbtPlugin := true
  )
    
  lazy val root = Project(id = "adept-sbt",
    base = file("."),
    settings = Project.defaultSettings ++ Seq(
      organization := "org.adept",
      version := "0.2.2-SNAPSHOT"
    )).dependsOn(file("../"))
}


