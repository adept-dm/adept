package adept.core

import java.io._
import org.eclipse.jgit.api.Git
import org.eclipse.jgit.api.errors.TransportException
import adept.core.operations._
import adept.core.models._
import akka.util.FiniteDuration
import adept.utils._
import org.eclipse.jgit.lib.{Tree => GitTree, _}
import org.eclipse.jgit.transport._
import com.jcraft.jsch.JSch

object Adept extends Logging {
  val AritifactPath = "artifacts"
  val RepositoriesPath = "repos"

  
  def dir(baseDir: File, name: String) = new File(new File(baseDir, RepositoriesPath), name)

  def open(baseDir: File, name: String): Either[String, Adept] = {
    if (exists(baseDir)) {
      Right(new Adept(dir(baseDir, name), name))
    } else {
      Left("no adept directory here: " + baseDir)
    }
  }

  def exists(baseDir: File): Boolean = {
    baseDir.exists && baseDir.isDirectory
  }

  def exists(baseDir: File, name: String): Boolean = {
    exists(baseDir) && {
      repositories(baseDir).find(_.name == name).isDefined
    }
  }

  def repositories(baseDir: File): List[Adept] = {
    val repoDir = new File(baseDir, RepositoriesPath)
    if (repoDir.exists && baseDir.isDirectory) {
      repoDir.listFiles().toList
        .filter(d => d != null && d.isDirectory)
        .map(d => new Adept(d, d.getName))
    } else {
      List.empty
    }
  }

  def clone(baseDir: File, name: String, uri: String): Either[String, Adept] = {
    val adeptDir = dir(baseDir, name)
    if (adeptDir.mkdirs()) {
      Git.cloneRepository()
         .setProgressMonitor(new TextProgressMonitor())
         .setURI(uri)
         .setDirectory(adeptDir)
         .call()
      Right(new Adept(adeptDir, name))
    } else {
      Left("could not create directory when cloning: " + adeptDir)
    }
  }

  def init(baseDir: File, name: String): Either[String, Adept] = {
    val adeptDir = dir(baseDir, name)
    if (adeptDir.mkdirs()) {
      val initCommand = Git.init()
       .setDirectory(adeptDir)
       initCommand.call()
       Right(new Adept(adeptDir, name))
    } else {
      Left("could not create directory when initing: " + adeptDir)
    }
  }



  def artifact(baseDir: File, info: Seq[((Hash, Set[String]), Option[File])], timeout: FiniteDuration) = { //TODO: Either[Seq[File], Seq[File]]  (left is failed, right is successful)
    val hashFiles = info.map{ case ((hash, locations), dest) =>
      (hash, locations, dest.getOrElse{
        val artifactDir = new File(baseDir, AritifactPath)
        artifactDir.mkdirs
        val firstLevelDir = hash.value.substring(0,2)
        val secondLevelDir = hash.value.substring(2,4)
        val currentArtifactDir = new File(new File(artifactDir, firstLevelDir), secondLevelDir) 
        ModuleFiles.createDir(currentArtifactDir)
        new File(currentArtifactDir , hash.value+".jar") //TODO: need a smarter way to store artifacts (imagine 50K jars in one dir!)
      })
    }
    val time = System.currentTimeMillis
    val (existing, nonExisting) = hashFiles.par.partition{ case (hash, locations, file) => //TODO: seems that .par has an effect, but could it be better if we used an IO context?
      file.exists && Hash.calculate(file) == hash
    }
    val timeSpent = System.currentTimeMillis - time
    logger.trace("spent " + timeSpent + " ms on checking sha1")
    for {
      existingFiles <- EitherUtils.reduce[String, File](existing.seq.map{ case (_, _, file ) => Right(file) }).right
      downloadedFiles <- EitherUtils.reduce[String, File](adept.download.Download(nonExisting.seq, timeout)).right
    } yield {
      existingFiles ++ downloadedFiles
    }
  }
 
  private[adept] type FindModule = (Coordinates, Option[UniqueId]) => Either[Set[Module], Option[Module]]
  
  def build(repositories: Set[Adept], confExpr: String, module: Module, 
      configurationMapping: String => String = Configuration.defaultConfigurationMapping(_)): Option[Tree] = {
    val findModule = MergeOperations.mergeFindModules(repositories)
    TreeOperations.build(confExpr, module, configurationMapping, findModule).map{ mutableTree =>
      ConflictResolver.resolveConflicts(mutableTree, configurationMapping, findModule)
      mutableTree.toTree
    }
  }
}

class Adept private[adept](val dir: File, val name: String) extends Logging {

  override def toString = {
    "Adept("+name+","+dir.getAbsolutePath+","+ lastCommit+")"
  }

  private lazy val git = Git.open(dir)

  private def gitRepo = git.getRepository()

  /* add module to adept. return right with file containing module, left with the file that could not be created*/
  def add(module: Module): Either[File, File] = {
    repo.Add(git, dir, module)
  }

  def findModule(coordinates: Coordinates, uniqueId: Option[UniqueId] = None): Either[Set[Module], Option[Module]] = {
    val file = new File(ModuleFiles.getModuleDir(dir, coordinates), ModuleFiles.modulesFilename)
    
    if (file.exists && file.isFile) {
      import org.json4s.native.JsonMethods._
      val maybeModules = Module.readSameCoordinates(parse(file))
      maybeModules.fold(
          error => throw new Exception(error),
          modules => uniqueId.map{ uniqueId =>
            val filtered = modules.filter(_.uniqueId == uniqueId)
            if (modules.size > 1) {
              logger.error("found more than 1 module with unique-id: " + uniqueId + " in " + file + " found: " + filtered)
              Left(modules.toSet)
            }
            else Right(filtered.headOption)
          }.getOrElse{
            if (modules.size > 1) {
              logger.error("found more than 1 module: " + coordinates + " in " + file + " found: " + modules)
              Left(modules.toSet)
            }
            else Right(modules.headOption)
          }
      )
    } else {
      Right(None)
    }
  }

  def lastCommit(allCoords: Set[Coordinates]): Option[Hash] = {
    val paths = allCoords.map{ coords =>
      new File(ModuleFiles.getModuleDir(dir, coords), ModuleFiles.modulesFilename)
    }.filter(f => f.exists && f.isFile).map{ file =>
      file.getAbsolutePath.replace(dir.getAbsolutePath + File.separatorChar, "")
    }.mkString(" ")

    val logIt = git.log()
                   .addPath(paths)
                   .call()
                   .iterator()
    if (logIt.hasNext()) Some(Hash(logIt.next.getName))
    else None
  }
  def lastCommit: Option[Hash] = {
    try {
      val logIt = git.log()
                     .call()
                     .iterator()
      Some(Hash(logIt.next.getName))
    } catch {
      case e: org.eclipse.jgit.api.errors.NoHeadException => None
    }
  }

  lazy val branchName = "master"

  def isLocal: Boolean = {
    try {
      gitRepo.getConfig().getString(
        ConfigConstants.CONFIG_BRANCH_SECTION, branchName,
        ConfigConstants.CONFIG_KEY_REMOTE)
      true
    } catch {
      case e: org.eclipse.jgit.api.errors.InvalidConfigurationException => false
    }
  }

  def pull(): Boolean = {
    val result = git
      .pull()
      .call()
    result.isSuccessful
  }

  def push(repo: String) = {
    val config = gitRepo.getConfig
    val remote = new RemoteConfig(config, "central")
    val uri = new URIish(repo)
    remote.addURI(uri)
    remote.update(config)
    config.save()
    try {
      SshSessionFactory.setInstance(GitHelpers.sshFactory)
      git.push.setRemote("central").call
    } catch {
      case x: TransportException => {
        println("ssh password required ...")
        SshSessionFactory.setInstance(GitHelpers.interactiveSshFactory)
        git.push.setRemote("central").call
      }
    }
  }

  def commit(msg: String) = {
    val status = git.status()
       .call()
    val noDiff = status.getChanged.isEmpty && status.getAdded.isEmpty
    if (noDiff) {
      Left("nothing to commit")
    } else {
    val revcommit = git
       .commit()
       .setMessage(msg)
       .call()

    Right(Hash(revcommit.name))
    }
  }

}
