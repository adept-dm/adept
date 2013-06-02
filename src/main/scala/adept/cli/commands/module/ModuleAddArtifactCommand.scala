package adept.cli.commands.module

import adept.cli.commands.Command
import adept.models._

import java.io._

object ModuleAddArtifactCommand extends Command with JsonFileSystemModulePersistance {

  val command = "add-artifact"
  val shortDescription = "add artifact to current module"
  override val help = Some(""" args: path-to-artifact.jar artifact-type locations* -- configs* """)

  def execute(args: List[String]): CommandResult = {
    val (artifact, artifactType, locations, configs) = parseArgs(args)

    for {
      module <- loadModule.right
      artifactFile <- checkArtifact(artifact).right
      unit <- checkConfigs(configs, module).right
    } yield {
      val newModule = updateModule(Artifact.fromFile(
        artifactFile,
        artifactType,
        configs,
        locations
      ), module)
      updatePersistedModule(_ => newModule)
      None
    }
  }

  def updateModule(artifact: Artifact, module: Module): Module =  {
    module.copy(
      artifacts = module.artifacts + artifact
    )
  }

  def parseArgs(args: List[String]) = {
    val (artifact :: artifactType :: tail) = args
    val (locations, configz) = tail.span( _ != "--")
    val configs = if (configz.length > 0) { configz.tail } else { Nil }

    (artifact, artifactType, locations.toSet, configs.toSet)
  }

  def checkArtifact(path: String): Either[String, File] = {
    val file = new File(path)
    if (file.exists && !file.isDirectory) {
      Right(file)
    } else {
      Left("%s does not exist or is a directory" format(path))
    }
  }

  def checkConfigs(configs: Set[String], module: Module): Either[String, Unit] = {
    if( configs.isEmpty) {
      Right()
    } else {
      val head = configs.head
      if ( module.configurations.find(_.name == head).isEmpty ) {
        Left("configuration %s is not a member of current module" format (head))
      } else {
        checkConfigs(configs.tail, module)
      }
    }
  }
}
