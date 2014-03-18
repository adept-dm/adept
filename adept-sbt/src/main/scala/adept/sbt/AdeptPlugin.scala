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
import adept.repository.models.LockFileArtifact

class AdeptRepository(baseDir: File) {
  import sbt.complete.DefaultParsers._

  def reposDir = {
    val reposDir = (baseDir / "repos")
    if (reposDir.exists() && reposDir.isDirectory()) {
      Some(reposDir)
    } else None
  }

  def repositories = reposDir.map { root =>
    if (root.isDirectory)
      root.listFiles().filter(_.isDirectory).map(_.getName)
    else Array.empty[String]
  }.getOrElse(Array.empty[String])

  def modules(repoName: String) = reposDir.map { d =>
    val variantsDir = (d / repoName / "variants")
    if (variantsDir.isDirectory)
      variantsDir.listFiles().filter(_.isDirectory).map(_.getName)
    else Array.empty[String]
  }.getOrElse(Array.empty[String])

  def repositoryParser: Parser[String] = {
    val candidates = repositories.toList.sorted
    if (candidates.size == 1) {
      candidates.head ^^^ candidates.head
    } else if (candidates.size > 1) {
      candidates.tail.foldLeft(candidates.head ^^^ candidates.head) { (currentParser, candidate) =>
        (candidate ^^^ candidate | currentParser)
      }
    } else {
      "--" ^^^ "--"
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

class SetAdeptCommand(repo: String, conf: String, id: String, constraints: Set[(String, Seq[String])], location: String, configurations: Set[ConfigurationId])(adeptManager: AdeptManager) extends AdeptCommand {
  import sbt.complete.DefaultParsers._

  def execute(): Unit = {
    adeptManager.set(repo, conf, id, constraints, location, configurations)
  }
}

class GetAdeptCommand(uri: String)(adeptManager: AdeptManager) extends AdeptCommand {
  import sbt.complete.DefaultParsers._

  def execute(): Unit = {
    adeptManager.get(uri)
  }
}

class AdeptManager(project: ProjectRef, baseDir: File, lockFile: File, passphrase: Option[String]) {
  val cacheManager = CacheManager.create()

  val UriRegEx = """.*/(.*)\.git""".r

  def get(uri: String) = {
    uri match {
      case UriRegEx(name) => //TODO: factor out into GitHelpers
        val dir = AdeptGitRepository.getRepoDir(baseDir, name)
        try {
          GitHelpers.withGitSshCredentials(passphrase) {
            import collection.JavaConverters._
            JGit
              .cloneRepository()
              .setCloneAllBranches(true)
              .setURI(uri)
              .setDirectory(dir)
              .call()
          }
        } catch {
          case e: Exception =>
            import scala.reflect.io.Directory
            Directory(dir).deleteRecursively
            throw new Exception(e)

        }
      case _ => throw new Exception("Cannot parse uri: " + uri + " with " + UriRegEx.pattern)
    }
  }

  def set(repo: String, conf: String, idString: String, constraints: Set[(String, Seq[String])], location: String, configurations: Set[ConfigurationId]) = {
    val id = Id(idString)
    val initTime = System.currentTimeMillis

    val thisGitRepo = new AdeptGitRepository(baseDir, repo)

    val parsedConstraints = constraints.map { case (name, values) => Constraint(name, values.toSet) }

    val newReqs = configurations.map { configuration =>
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
      val commits = GitLoader.loadCommits(baseDir, requirements, cacheManager, location, passphrase)

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
        val artifactRefs = variants.flatMap { case (_, variant) => variant.artifacts }.toSet
        val artifacts = gitLoader.getArtifacts(artifactRefs).map {
          case (a, ar) =>
            LockFileArtifact(a.hash, a.size, a.locations, ar.filename)
        }
        LockFile(currentHash, requirements.toSeq.sortBy(_.repositoryName).sortBy(_.id.value), artifacts.toSeq).write(lockFile)
      }
    }
  }
}

class Progress {

  def clearMessage() = {
    print("\r" * message.size)
    print(" " * message.size)
    print("\r" * message.size)
  }
  
  var totalChunks = 0L
  var currentProgress = 0L
  var message = ""
  var totalFiles = 0
  var files = Set.empty[String]
  var filesCompleted = 0
  
  def registerFile(chunks: Long, filename: String) = synchronized {
    totalChunks += chunks
    totalFiles += 1
    files += filename
  }
  
  def fileFinished(chunks: Long, filename: String) = synchronized { //TODO: :( bad shit...
    clearMessage()
    currentProgress += chunks
    filesCompleted += 1
    files -= filename
    
    message = 
      f"${(100 * currentProgress.toDouble) / totalChunks.toDouble}%1.0f%% (${currentProgress/(1024*1024)}/${totalChunks/(1024*1024)}mb, $filesCompleted/$totalFiles) - " + (if (files.size == 1) s"waiting for ${files.head}" else s"$filename completed")
    if (message.size > 80) {
      message = message.slice(0, 77) + "..."
    }
    print(message)
    if (currentProgress >= totalChunks) {
      clearMessage()
      println(f"${totalChunks.toDouble / (1024*1024)}%1.1fmb and $filesCompleted files downloaded!")
    }
  }
}

object AdeptPlugin extends Plugin {

  val MasterConfiguration = "master"
  import AdeptKeys._

  
  private def getLocation(metadataLocations: Set[String]) = { //TODO: we should be able to support more than one... 
    if (metadataLocations.size != 1) throw new Exception("Only EXACTLY one metadata location is currently supported. Found: " + metadataLocations)
    else metadataLocations.head
  }
  
  def adeptSettings = Seq(
    adeptDirectory := {
      val log = streams.value.log
      println(ivyModule.value.dependencyMapping(log))
      println(ivyModule.value.defaultConfig(log))
      println(ivyModule.value.moduleDescriptor(log))
      println(ivyModule.value.moduleSettings)
      
      Path.userHome / ".adept"
    },
    adeptConfigurations := Set("compile", "master"),
    //adeptConfigurations in Test := Set("test", "compile", "master"),
    adeptLockFile := new File(baseDirectory.value, "adept.lock"),
    adeptRequirements := {
      val lockFile = adeptLockFile.value
      if (lockFile.exists) {
        LockFile.read(lockFile).requirements
      } else Seq.empty
    },
    adeptSshPassphrase := None,
    adeptClasspath := {
      import scala.concurrent.ExecutionContext.Implicits._
      import scala.concurrent._
      val lockFile = adeptLockFile.value
      if (lockFile.exists) {
        val lockFileData = LockFile.read(lockFile)
        val progress = new Progress

        val futures = lockFileData.artifacts.map { artifact =>
          //val artifactFiles = variants.flatMap { case (_, variant) => variant.artifacts.map(a =>  -> a.filename) }.toSeq
          //        val artifactFiles = variants.flatMap { case (_, variant) => variant.artifacts.map(a =>  -> a.filename) }.toSeq
          val filename = artifact.filename.getOrElse(throw new Exception("Currently not implemented artifact that does not specify filename")) //TODO: use artifact.hash for default?
          ArtifactCache.getOrCreateExistingCacheFile(adeptDirectory.value, artifact.hash, filename) match {
            case Some(cacheFile) => Future(cacheFile)
            case None =>
              progress.registerFile(artifact.size, filename)
              val f = Downloader.download(artifact.locations, artifact.hash, ArtifactCache.cacheFile(adeptDirectory.value, artifact.hash, filename), new File(System.getProperty("java.io.tmpdir"))).map(r => ArtifactCache.cache(adeptDirectory.value, r._1, r._2, filename)) //TODO: factor
              f.onSuccess {
                case _ =>
                  progress.fileFinished(artifact.size, filename)
              }
              f
          }
        }
        import scala.concurrent.duration._
        val cacheFiles = Await.result(Future.sequence(futures), 60.minutes) //TODO: configurable timeout
        cacheFiles.map(Attributed.blank(_))
      } else Seq.empty
    },
    sbt.Keys.commands += {
      val SetCommand = token("set")
      val GetCommand = token("get")
      val GraphCommand = token("graph")
      val IvyImport = token("ivy-import")

      val RepositorySep = token("/")
      val adeptManager = new AdeptManager(thisProjectRef.value, adeptDirectory.value, adeptLockFile.value, adeptSshPassphrase.value) //TODO: settings!
      val adeptRepository = new AdeptRepository(adeptDirectory.value)
      val configurations = adeptConfigurations.value.map(ConfigurationId(_))
      def repositoires = token(Space ~> adeptRepository.repositoryParser) flatMap { repo =>
        token(RepositorySep ~> adeptRepository.idParser(repo)).flatMap { id =>
          token((Space ~> charClass(_ => true, "1").*) | charClass(_ => true, "2").*).map { binaryVersionsChars =>
            val binaryVersionsString = binaryVersionsChars.mkString
            val binaryVersion = binaryVersionsString.trim().split(" ").map(_.trim()).filter(_.nonEmpty)
            val constraints =
              if (binaryVersion.isEmpty)
                Set.empty[(String, Seq[String])]
              else
                Set(AttributeDefaults.BinaryVersionAttribute -> binaryVersion.toSeq)

            new SetAdeptCommand(repo, "compile", id, constraints, getLocation(adeptMetadataLocations.value), configurations)(adeptManager)
          }
        }
      }

      def set = SetCommand ~> repositoires

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

      def graph = GraphCommand.map { _ =>
        new AdeptCommand {
          def execute() = {
            val graphString = {
              val lockFile = adeptLockFile.value
              val requirements =
                if (lockFile.exists) {
                  LockFile.read(lockFile).requirements
                } else Seq.empty
              if (requirements.nonEmpty) { //TODO: factor this out!
                val initTime = System.currentTimeMillis
                val loadingMsg = "Loading..."
                print(loadingMsg)
                
                val commits = GitLoader.loadCommits(adeptDirectory.value, requirements.toSet, adeptManager.cacheManager, getLocation(adeptMetadataLocations.value), adeptSshPassphrase.value)

                val loadedTime = System.currentTimeMillis
                val resolvingMsg = "Loaded (" + (loadedTime - initTime) + "ms). Resolving..."
                print(("\b" * loadingMsg.size) + resolvingMsg)
                val gitLoader = new GitLoader(commits, cacheManager = adeptManager.cacheManager)
                val gitResolver = new Resolver(gitLoader)
                val result = gitResolver.resolve(requirements.map(_.asRequirement).toSet)
                println("Finished (" + (System.currentTimeMillis - initTime) + "ms)")
                result.graphAsString
              } else ""
            }
            println(graphString)
          }
        }

      }

      def adept = (Space ~> (set | get | ivyImport | graph))

      Command("adept")(_ => adept) { (state, adeptCommand) =>
        adeptCommand.execute()
        state
      }
    })
}