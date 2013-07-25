package adept.sbt

import sbt.{ Configuration => _, Node => _, Artifact => _, _ }
import sbt.Keys._
import adept.core.models._
import adept.core.Adept
import adept.ivy._
import adept.sbt.tasks._
import akka.util.duration._

object AdeptPlugin extends Plugin
  with AdeptAdd
  with AdeptClasspath
  with AdeptIvyAdd
  with AdeptModule
  with AdeptRepository
  with AdeptTree
  with AdeptUpdate {
  import AdeptKeys._

  def adeptSettings = Seq(
    adeptConfigurationMapping := "compile->compile(*),master(*);runtime->runtime(*)",
    adeptDirectory := Path.userHome / ".adept",
    adeptTimeout := 60, //minutes
    adeptRepositories := Map(),
    adeptDependencies := Seq(),
    adeptLocalRepositoryName := "local",
    adeptLocalRepository <<= adeptLocalRepositoryTask,
    adeptIvyAdd <<= adeptIvyAddTask,
    //TODO: find a way to do this for all configurations - this is not good!
    adeptTree in Compile <<= adeptTreeTask(Compile),
    adeptTree in Runtime <<= adeptTreeTask(Runtime),
    adeptTree in Test <<= adeptTreeTask(Test),
    adeptClasspath in Compile <<= adeptClasspathTask(Compile),
    adeptClasspath in Runtime <<= adeptClasspathTask(Runtime),
    adeptClasspath in Test <<= adeptClasspathTask(Test),
    (managedClasspath in Compile) <++= adeptClasspath in Compile,
    (managedClasspath in Runtime) <++= adeptClasspath in Runtime,
    (managedClasspath in Test) <++= adeptClasspath in Test,
    adeptUpdate <<= adeptUpdateTask,
    adeptModule <<= adeptModuleTask,
    adeptArtifactLocations := Map.empty,
    adeptAdd <<= adeptInputAddTask)
}
