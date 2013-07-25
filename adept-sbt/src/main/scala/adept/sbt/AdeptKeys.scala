package adept.sbt 

import sbt._
import sbt.Keys._

import adept.core.Adept
import adept.core.models._

object AdeptKeys {
  val adeptDependencies         = SettingKey[Seq[ModuleID]]("adept-dependencies", "declares dependencies fetched by adept")
  val adeptTree                 = TaskKey[Option[Tree]]("adept-tree", "the adept dependency tree")
  val adeptRepositories         = SettingKey[Map[String,String]]("adept-repositories", "adept the name and git url for the adept repositories")
  val adeptLocalRepository      = TaskKey[Option[Adept]]("adept-local-repository", "the local repository for adept")
  val adeptTimeout              = SettingKey[Int]("adept-timeout", "timeout for downloads in minutes for adept")
  val adeptClasspath            = TaskKey[Classpath]("adept-classpath", "the classpath generated from adept tree")
  val adeptIvyAdd               = TaskKey[Seq[Module]]("adept-ivy-add", "uses Ivy with the modules in libraryDependencies and adds them to the local adept repository")
  val adeptAdd                  = InputKey[Seq[(Module,File)]]("adept-add", "add current module repositories provided by arguments")
  val adeptDirectory            = SettingKey[File]("adept-directory", "the adept home directory")
  val adeptConfigurationMapping = SettingKey[String]("adept-configuration-mapping", "the default configuration mapping used by adept. example: *->default(compile), maps: 'test' configuration to 'test->default(compile)'")
  val adeptModule               = TaskKey[Option[Module]]("adept-module", "the current module")
  val adeptUpdate               = TaskKey[Seq[Adept]]("adept-update", "updated adept repositories") 
  val adeptLocalRepositoryName  = TaskKey[String]("adept-local-repository-name", "local repository name")
  val adeptArtifactLocations    = SettingKey[Map[String, String]]("adept-artifact-locations", "if modules are already published use this to say where each type of artifact (jar, source, javadoc, ...) artifacts are located")   
}
