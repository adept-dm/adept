import sbt._
import sbt.Keys._
import sbtrelease.ReleasePlugin._

object AdeptBuild extends Build {
  
  val commonSettings = Seq(
    scalaVersion := "2.9.2",
    organization := "org.adept",
    version := {
      val format = new java.text.SimpleDateFormat("YYYYMMddHHmmss")
      "0.8.0-ALPHA-"+(format.format(new java.util.Date))
    }
  ) ++ releaseSettings

  def AdeptProject(name: String) = Project(name, file(name)).settings(commonSettings: _*)

  lazy val adeptCore = AdeptProject("adept-core")
    .settings((
    Dependencies.akka ++
    Dependencies.logback ++
    Dependencies.spray ++
    Dependencies.git ++
    Dependencies.json4s ++
    Dependencies.ivy ++
    Dependencies.scalaTest
    ):_*)

  lazy val adeptSbt = AdeptProject("adept-sbt")
    .settings(
    sbtPlugin := true
    ).dependsOn(adeptCore)

  lazy val adeptCli = AdeptProject("adept-cli")
    .settings((
      Dependencies.scalaTest
    ): _*).dependsOn(adeptCore)


  lazy val root = Project("adept", file(".")).settings(commonSettings: _*)
    .settings(
      publishArtifact := false
    )
    .aggregate(adeptCore, adeptSbt, adeptCli)

}
