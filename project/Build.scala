import sbt._
import sbt.Keys._

object AdeptBuild extends Build {
  
  val commonSettings = Seq(
    scalaVersion := "2.9.2",
    organization := "org.adept",
    version := "0.1"
  )

  def AdeptProject(name: String) = Project(name, file(name)).settings(commonSettings: _*)


  lazy val adeptCore = AdeptProject("adept-core")
    .settings((
    Dependencies.akka ++
    Dependencies.logback ++
    Dependencies.spray ++
    Dependencies.git ++
    Dependencies.json4s ++
    Dependencies.ivy
    ):_*)

  lazy val adeptCli = AdeptProject("adept-cli")
    .dependsOn(adeptCore)

  lazy val adeptSbt = AdeptProject("adept-sbt")
    .settings(
    sbtPlugin := true
    ).dependsOn(adeptCore)

  lazy val adeptTools = AdeptProject("adept-tools")

  lazy val root = Project("adept", file("."))
    .aggregate(adeptCore, adeptCli, adeptSbt, adeptTools)

}
