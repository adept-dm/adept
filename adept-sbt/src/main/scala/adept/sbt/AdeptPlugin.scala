package adept.sbt

import sbt.{ Id => _, _ }
import sbt.Keys._
import sbt.complete.DefaultParsers._
import sbt.complete._
import play.api.libs.json.Json
import adept.repository.AdeptGitRepository
import adept.models._
import adept.repository.models.configuration.ConfiguredRequirement
import adept.repository.AdeptCommit
import adept.resolution.Resolver
import adept.repository.GitLoader
import adept.repository.models.Commit
import adept.repository.models.configuration.ConfiguredRequirement
import adept.repository.models.configuration.ConfigurationId
import net.sf.ehcache.CacheManager
import adept.repository.models.RepositoryMetadata
import adept.repository.models.LockFileRequirement
import adept.repository.models.configuration.ConfiguredRequirement
import adept.resolution.models.UnderconstrainedResult
import adept.resolution.models.ResolvedResult
import adept.resolution.models.OverconstrainedResult
import adept.resolution.models.ResolveResult
import adept.artifacts.ArtifactCache
import adept.models.Hash
import adept.repository.models.configuration.ConfigurationId
import adept.repository.models.LockFileRequirement
import adept.repository.models.LockFile
import adept.ext.AttributeDefaults
import org.eclipse.jgit.api.{ Git => JGit }
import org.eclipse.jgit.transport.JschConfigSessionFactory
import org.eclipse.jgit.transport.OpenSshConfig
import org.eclipse.jgit.transport.CredentialsProvider
import org.eclipse.jgit.transport.CredentialItem
import org.eclipse.jgit.transport.URIish
import org.eclipse.jgit.transport.CredentialsProviderUserInfo
import org.eclipse.jgit.transport.SshSessionFactory
import adept.repository.GitHelpers
import adept.artifacts.Downloader

class AdeptRepository(baseDir: File) {
  import sbt.complete.DefaultParsers._

  def reposDir = {
    val reposDir = (baseDir / "repos")
    if (reposDir.exists() && reposDir.isDirectory()) {
      Some(reposDir)
    } else None
  }

  def repositories = reposDir.map {
    _.listFiles().filter(_.isDirectory).map(_.getName)
  }.getOrElse(Array.empty)

  def modules(repoName: String) = reposDir.map { d =>
    (d / repoName / "variants").listFiles().filter(_.isDirectory).map(_.getName)
  }.getOrElse(Array.empty)

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

class SetAdeptCommand(repo: String, conf: String, id: String, constraints: Set[(String, Seq[String])])(adeptManager: AdeptManager) extends AdeptCommand {
  import sbt.complete.DefaultParsers._

  def execute(): Unit = {
    adeptManager.set(repo, conf, id, constraints)
  }
}

class GetAdeptCommand(uri: String)(adeptManager: AdeptManager) extends AdeptCommand {
  import sbt.complete.DefaultParsers._

  def execute(): Unit = {
    adeptManager.get(uri)
  }
}

object ResultStore { //TODO: it would be best if we could avoid this of course
  def updateResult(project: ProjectRef, resolveResult: ResolveResult) = synchronized { //TODO: can be moved to per project lock not on all?
    results += project -> resolveResult
  }

  def getResult(project: ProjectRef) = synchronized { //TODO: can be moved to per project lock not on all?
    results.get(project)
  }

  private var results: Map[ProjectRef, ResolveResult] = Map.empty

}

object Helper { //TODO: remove this and put it in adpet-core 
  val FAKE_PATH = "https://github.com/adept-test-repo1/"

}

class AdeptManager(project: ProjectRef, baseDir: File, lockFile: File, passphrase: Option[String]) {
  val cacheManager = new CacheManager()
  val DefaultConfigurations = Set(ConfigurationId("compile"), ConfigurationId("master"))

  val UriRegEx = """.*/(.*)\.git""".r

  def get(uri: String) = {
    uri match {
      case UriRegEx(name) => //TODO: factor out into GitHelpers
        GitHelpers.withGitSshCredentials(passphrase) {
          JGit
            .cloneRepository()
            .setURI(uri)
            .setDirectory(AdeptGitRepository.getRepoDir(baseDir, name))
            .call()
        }
      case _ => throw new Exception("Cannot parse uri: " + uri + " with " + UriRegEx.pattern)
    }
  }

  def set(repo: String, conf: String, idString: String, constraints: Set[(String, Seq[String])]) = {
    val id = Id(idString)
    val initTime = System.currentTimeMillis

    val thisGitRepo = new AdeptGitRepository(baseDir, repo)

    val currentConfigurations = DefaultConfigurations //TODO: replace with setting
    val parsedConstraints = constraints.map { case (name, values) => Constraint(name, values.toSet) }

    val newReqs = currentConfigurations.map { configuration =>
      LockFileRequirement(id, configuration, parsedConstraints.toSeq, thisGitRepo.name, thisGitRepo.getMostRecentCommit.commit) //TODO: we should not use most recent commit but the first one that matches the constraints
    }

    val currentLockFile = if (lockFile.exists && lockFile.isFile) {
      Some(LockFile.read(lockFile))
    } else None

    val oldRequirements = currentLockFile match {
      case Some(lockFile) =>
        lockFile.requirements.filter(_.id != id).toSet
      case None => Set.empty
    }
    val requirements = newReqs ++ oldRequirements

    val currentHash = Hash.calculate(requirements)
    if (Some(currentHash) == currentLockFile.map(_.hash)) {
      println("Using lockfile: " + lockFile)
    } else {
      val commits = GitLoader.loadCommits(baseDir, requirements, cacheManager, Helper.FAKE_PATH, passphrase)

      val loadedTime = System.currentTimeMillis
      val resolvingMsg = "Loaded (" + (loadedTime - initTime) + "ms). Resolving..."
      print(resolvingMsg)

      val gitLoader = new GitLoader(commits, cacheManager = cacheManager)
      val gitResolver = new Resolver(gitLoader)
      val result = gitResolver.resolve(requirements.map(_.asRequirement).toSet)
      val timeString = "resolved in: " + (System.currentTimeMillis - loadedTime) + "ms, loaded in: " + (loadedTime - initTime) + "ms"
      val resultString =
        result match {
          case _: ResolvedResult =>
            "Completed (" + timeString + ")"
          case _: OverconstrainedResult =>
            val requiredIds = newReqs.map(_.id)
            val currentOverconstrained = result.state.overconstrained.filter(requiredIds)
            val displayErrorIds = if (currentOverconstrained.isEmpty) result.state.overconstrained else currentOverconstrained
            val help = displayErrorIds.map { id =>
              if (gitLoader.loadVariants(id, result.state.constraints(id)).isEmpty) {
                if (gitLoader.loadVariants(id, Set.empty).isEmpty) {
                  id + " cannot be found in repositories: " + commits.map { case (id, c) => c.repo.name }.mkString(" or ")
                } else {
                  id + result.state.constraints(id).map(c => c.name + "=" + c.values.mkString("(", ",", ")")).mkString(" with ", " and ", " does not exist")
                }
              } else {
                id + " conflicts " + result.state.constraints(id).map(c => c.name + "=" + c.values.mkString("(", ",", ")")).mkString(",")
              }
            }.mkString("\n")
            "Over-constrained (" + timeString + "):\n" + help
          case underConstrainedResult: UnderconstrainedResult =>
            val help = "Choose between:\n" + (if (underConstrainedResult.optimalStates.nonEmpty) {
              underConstrainedResult.optimalStates.map(s => (s.implicitVariants ++ s.resolvedVariants).flatMap {
                case (foundId, foundVariant) =>
                  if (result.state.underconstrained(foundId)) Some(foundVariant)
                  else None
              }.toSet): Set[Set[Variant]]
            } else {
              result.state.underconstrained.map(id => gitLoader.loadVariants(id, parsedConstraints)): Set[Set[Variant]]
            }).map(_.map(v => "(" + v.id + ": " + v.attributes.map(a => a.name + "=" + a.values.mkString("(", ",", ")")).mkString("<", ",", ">") + ")").mkString(" OR ")).mkString("\n")
            "Under-constrained (" + timeString + "): " + result.state.underconstrained.mkString(",") + ":\n" + help
        }
      println(("\b" * resolvingMsg.size) + resultString)

      if (result.isResolved) { //TODO: I am not sure whether it is right to only store result if resolvd (if we are under-constrained it would be nice to increase precision..)
        val variants = result.state.implicitVariants ++ result.state.resolvedVariants
        val hashes = variants.flatMap { case (_, variant) => variant.artifacts.map(_.hash) }.toSet
        val artifacts = gitLoader.getArtifacts(hashes)
        LockFile(currentHash, requirements.toSeq.sortBy(_.repositoryName).sortBy(_.id.value), artifacts.toSeq).write(lockFile)
      }

      ResultStore.updateResult(project, result)
    }
  }
}

object AdeptPlugin extends Plugin {

  import AdeptKeys._

  def adeptSettings = Seq(
    adeptDirectory := Path.userHome / ".adept",
    adeptLockFile := new File(baseDirectory.value, "adept.lock"),
    adeptSshPassphrase := None,
    adeptClasspath := {
      import scala.concurrent.ExecutionContext.Implicits._
      import scala.concurrent._
      val lockFile = adeptLockFile.value
      if (lockFile.exists) {
        val futures = LockFile.read(lockFile).artifacts.map { artifact =>
          //val artifactFiles = variants.flatMap { case (_, variant) => variant.artifacts.map(a =>  -> a.filename) }.toSeq
          //        val artifactFiles = variants.flatMap { case (_, variant) => variant.artifacts.map(a =>  -> a.filename) }.toSeq

          val cacheFile = ArtifactCache.getCacheFile(adeptDirectory.value, artifact.hash)
          if (cacheFile.exists()) Future(cacheFile)
          else Downloader.download(artifact.locations, artifact.hash, cacheFile, new File(System.getProperty("java.io.tmpdir"))).map(r => ArtifactCache.cache(adeptDirectory.value, r._1, r._2)) //TODO: factor
        }
        import scala.concurrent.duration._
        Await.result(Future.sequence(futures), 60.minutes) //TIMEOUT
      } else Seq.empty
    },
    sbt.Keys.commands += {
      val CurrentSetCommand = token("adfdas")
      val GetCommand = token("get")
      val GraphCommand = token("graph")
      val IvyImport = token("ivy-import")

      val RepositorySep = token("/")
      val adeptManager = new AdeptManager(thisProjectRef.value, adeptDirectory.value, adeptLockFile.value, adeptSshPassphrase.value) //TODO: settings!
      val adeptRepository = new AdeptRepository(adeptDirectory.value)
      val repositoires = token(Space ~> adeptRepository.repositoryParser) flatMap { repo =>
        token(RepositorySep ~> adeptRepository.idParser(repo)).flatMap { id =>
          token((Space ~> charClass(_ => true, "1").*) | charClass(_ => true, "2").*).map { binaryVersionsChars =>
            val binaryVersionsString = binaryVersionsChars.mkString
            val binaryVersion = binaryVersionsString.trim().split(" ").map(_.trim()).filter(_.nonEmpty)
            val constraints =
              if (binaryVersion.isEmpty)
                Set.empty[(String, Seq[String])]
              else
                Set(AttributeDefaults.BinaryVersionAttribute -> binaryVersion.toSeq)

            new SetAdeptCommand(repo, "compile", id, constraints)(adeptManager)
          }
        }
      }

      val currentSet = CurrentSetCommand ~> repositoires

      val get = GetCommand ~> (Space ~> charClass(_ => true, "uri").+).map { uriChars =>
        val uri = uriChars.mkString
        new GetAdeptCommand(uri)(adeptManager)
      }

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

      val graph = GraphCommand.map { _ =>
        new AdeptCommand {
          def execute() = {
            println(ResultStore.getResult(thisProjectRef.value) match {
              case Some(result) => result.graphAsString
              case None => ""
            })
          }
        }

      }
      val adept = (Space ~> (currentSet | get | ivyImport | graph))

      Command("adept")(_ => adept) { (state, adeptCommand) =>
        adeptCommand.execute()
        state
      }
    })
}