import sbt._
import sbt.Keys._
import sbtrelease.ReleasePlugin._

object AdeptBuild extends Build {

  val commonSettings = Seq(
    scalaVersion := "2.9.2",
    organization := "org.adept",
    version := {
      val format = new java.text.SimpleDateFormat("yyyyMMddHHmmss")
      "0.8.0-ALPHA-"+(format.format(new java.util.Date))
    }
  ) ++ releaseSettings

  def AdeptProject(name: String) = Project(name, file(name), configurations = Configurations.default ++ Seq(Configuration("default", "runtime dependencies and master artifact can be used with this conf", isPublic = true, extendsConfigs = List(config("runtime"), config("master")), transitive = true), Configuration("master", "contains only the artifact published by this module itself, with no transitive dependencies", isPublic = true, extendsConfigs = List(), transitive = true)) ).settings(commonSettings: _*)

  lazy val adeptCore = AdeptProject("adept-core")
    .settings((
    Dependencies.akka ++
    Dependencies.logback ++
    Dependencies.spray ++
    Dependencies.git ++
    Dependencies.json4s ++
    Dependencies.ivy ++
    Dependencies.ehcache ++
    Dependencies.scalaTest
    ):_*)

  lazy val adeptExamples = Project("adept-core-examples", file("adept-core") / "examples")
    .dependsOn(adeptCore)

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
