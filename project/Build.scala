import sbt._
import sbt.Keys._

object AdeptBuild extends Build {
  
  val commonSettings = Seq(
    
  )

  def AdeptProject(name: String) = Project(name, file(name)).settings(commonSettings: _*)


  lazy val adeptCore = AdeptProject("adept-core")
    .settings((
    Dependencies.akka ++
    Dependencies.logback ++
    Dependencies.spray ++
    Dependencies.git ++
    Dependencies.json4s
    ):_*).dependsOn(adeptIvy)

  lazy val adeptCli = AdeptProject("adept-cli")
    .dependsOn(adeptCore)

  lazy val adeptIvy = AdeptProject("adept-ivy")
    .settings((
      Dependencies.ivy
    ):_*)

  lazy val adeptSbt = AdeptProject("adept-sbt")
    .settings(
    sbtPlugin := true
    ).dependsOn(adeptCore)

  lazy val adeptSbt = AdeptProject("adept-sbt")

  lazy val root = Project("root", file(".")).aggregate(adeptCore, adeptCli, adeptIvy, adeptSbt, adeptTools)

}
