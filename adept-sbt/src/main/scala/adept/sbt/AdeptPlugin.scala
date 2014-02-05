package adept.sbt

import sbt._
import sbt.Keys._
import sbt.complete.DefaultParsers._
import sbt.complete._
import play.api.libs.json.Json
import adept.repository.AdeptGitRepository
import adept.models._
import adept.repository.models.configuration.ConfiguredRequirement
import adept.repository.AdeptCommit
import adept.resolution.Resolver
import adept.repository.GitVariantsLoader

object AdeptRepository {
  import sbt.complete.DefaultParsers._

  val repositories = Set[String](
    "com.typesafe.akka",
    "org.eclipse.jgit",
    "com.typesafe.play")

  val modules = Map(
    "com.typesafe.akka" -> Set("akka-actor", "akka-remote"),
    "org.eclipse.jgit" -> Set("org.eclipse.jgit"),
    "com.typesafe.play" -> Set("play", "play-json"))

  def repositoryParser: Parser[String] = {
    val candidates = repositories.toList.sorted
    if (candidates.size == 1) {
      candidates.head ^^^ candidates.head
    } else if (candidates.size > 1) {
      candidates.tail.foldLeft(candidates.head ^^^ candidates.head) { (currentParser, candidate) =>
        (candidate ^^^ candidate | currentParser)
      }
    } else {
      Parser.failure("No repositories found")
    }
  }

  def idParser(repository: String): Parser[String] = {
    import sbt.complete.DefaultParsers._
    val candidates = modules(repository).toList.sorted
    if (candidates.size == 1) {
      candidates.head ^^^ candidates.head
    } else if (candidates.size > 1) {
      candidates.tail.foldLeft(candidates.head ^^^ candidates.head) { (currentParser, candidate) =>
        (candidate ^^^ candidate | currentParser)
      }
    } else {
      Parser.failure("No ids found")
    }
  }
}

abstract class AdeptCommand {
  def execute(): Unit
}

class IvyImportCommand(org: String, name: String, revision: String) extends AdeptCommand {
  def execute(): Unit = {
    println("ivy import: " + org + ", " + name + ", " + revision)
  }
}

class InstallAdeptCommand(repo: String, id: String, constraints: Set[(String, String)])(adeptManager: AdeptManager) extends AdeptCommand {
  import sbt.complete.DefaultParsers._

  def execute(): Unit = {
    adeptManager.install(repo, id, constraints)
  }
}

package lockfile {
  package object serialization {
    implicit val configurationEntryFormat = Json.format[ConfigurationEntry]
    implicit val requirementEntryFormat = Json.format[RequirementEntry]
    implicit val artifactEntryFormat = Json.format[ArtifactEntry]
    implicit val lockfileFormat = Json.format[LockFile]
  }
  case class ConfigurationEntry(id: String, constraints: Set[Map[String, Seq[String]]], repository: String, commit: String)
  case class RequirementEntry(hash: String, configurations: Map[String, Seq[ConfigurationEntry]])

  case class ArtifactEntry(hash: String, variant: String, locations: Seq[String], size: Long)
  case class LockFile(requirements: Seq[RequirementEntry], artifacts: Seq[ArtifactEntry]) {
    def getResolveInfo: (Set[ConfiguredRequirement], Set[AdeptCommit]) = {
      ???
    }
  }
}

object LockFile {

  def read(file: File) = {
    if (file.exists() && file.isFile) {
      val source = io.Source.fromFile(file)
      import lockfile.serialization._
      Json.parse(source.getLines.mkString("\n")).as[lockfile.LockFile]
    } else lockfile.LockFile(Seq.empty, Seq.empty)
  }
}

class AdeptManager(baseDir: File, lockFile: File) {

  def install(repo: String, conf: String, id: String, constraints: Set[(String, Seq[String])]) = {
    val gitRepo = new AdeptGitRepository(baseDir, repo)
    val (configuredRequirements, commits) = LockFile.read(lockFile).getResolveInfo
     
    val gitVariantsLoader = new GitVariantsLoader(commits, cacheManager = null)
    val gitResolver = new Resolver(gitVariantsLoader)
    
    val requirements = configuredRequirements.flatMap(_.asRequirements)

    val result = gitResolver.resolve(requirements)
    
    if (result.isResolved) {
      
      LockFile.write()
    } else {
      
    }
    //read lockfile and use all commits, (id, constraints)s and resolve
    
    //if resolves then create a new lockfile
    //else fail
  }
}

object AdeptPlugin extends Plugin {

  import AdeptKeys._

  def adeptSettings = Seq(
    adeptDirectory := Path.userHome / ".adept",
    sbt.Keys.commands += {

      val lockFile = LockFile.read(new File("adept.lock"))

      val Install = token("install")
      val IvyImport = token("ivy-import")

      val RepositorySep = token("/")
      val adeptManager = new AdeptManager(adeptDirectory.value)

      val repositoires = token(Space ~> AdeptRepository.repositoryParser) flatMap { repo =>
        token(RepositorySep ~> AdeptRepository.idParser(repo)).map { id =>
          new InstallAdeptCommand(repo, id, Set.empty)(adeptManager)
        }
      }

      val install = Install ~> repositoires

      val ivyImport = (IvyImport ~> (Space ~> charClass(_ => true, "ivy-organization").+.flatMap { orgChars =>
        val org = orgChars.mkString
        token("#") ~> charClass(_ => true, "ivy-name").+.flatMap { nameChars =>
          val name = nameChars.mkString
          token(";") ~> charClass(_ => true, "ivy-revision").+.map { revisionChars =>
            val revision = revisionChars.mkString
            new IvyImportCommand(org, name, revision)
          }
        }
      }))

      val adept = (Space ~> (install | ivyImport))

      Command("adept")(_ => adept) { (state, adeptCommand) =>
        adeptCommand.execute()
        state
      }
    })
}