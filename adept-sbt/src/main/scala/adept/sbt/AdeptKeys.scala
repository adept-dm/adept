import sbt._
import sbt.Keys._

import adept.core.Adept
import adept.core.models._

object AdeptKeys {
  val defaultArtifactTypes = Seq("jar", "bundle")
  //TODO: adept-describe-classpath
  val adeptDependencies         = SettingKey[Seq[ModuleID]]("adept-dependencies", "declares dependencies fetched by adept")
  val adeptRepositories         = SettingKey[Map[String,String]]("adept-repositories", "adept the name and git url for the adept repositories")
  val adeptLocalRepository      = TaskKey[Option[Adept]]("adept-local-repository", "the local repository for adept")
  val adeptTimeout              = SettingKey[Int]("adept-timeout", "timeout for downloads in minutes for adept")
  val adeptClasspath            = TaskKey[Classpath]("adept-classpath", "the classpath generated from adept")
  val adeptIvyAdd               = TaskKey[Seq[Module]]("adept-ivy-add", "uses Ivy with the modules in libraryDependencies and adds them to the local adept repository")
  val adeptDirectory            = SettingKey[File]("adept-directory", "the adept home directory")
  val adeptArtifactTypes        = SettingKey[Seq[String]]("adept-artifact-types", "artifact types that adept accepts. default: " + defaultArtifactTypes.mkString(","))
  val adeptConfigurationMapping = SettingKey[String]("adept-configuration-mapping", "the default configuration mapping used by adept. example: *->default(compile), maps: 'test' configuration to 'test->default(compile)'")
}
