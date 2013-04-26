package adept

import java.io._
import org.eclipse.jgit.api.Git
import adept.operations._
import adept.models._
import akka.util.FiniteDuration
import org.slf4j.LoggerFactory
import adept.utils.EitherUtils
import org.eclipse.jgit.lib.Constants
import org.eclipse.jgit.lib.ConfigConstants
import org.eclipse.jgit.lib.TextProgressMonitor

object Adept {
  def dir(baseDir: File, name: String) = new File(baseDir, name)
  
  def open(baseDir: File, name: String): Either[String, Adept] = {
    Right(new Adept(dir(baseDir, name), name))
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
    if (baseDir.exists && baseDir.isDirectory) {
      baseDir.listFiles().toList
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
  
    
  val aritifactPath = "artifacts"
  
  def artifact(baseDir: File, info: Seq[((Hash, Coordinates, Set[String]), Option[File])], timeout: FiniteDuration) = { //TODO: Either[Seq[File], Seq[File]]  (left is failed, right is successful)
    val hashFiles = info.map{ case ((hash, coords, locations), dest) =>
      (hash, coords, locations, dest.getOrElse{
        val artifactDir = new File(baseDir, aritifactPath)
        artifactDir.mkdirs
        val currentArtifactDir = Add.getModuleDir(artifactDir, coords)
        Add.createDir(currentArtifactDir)
        new File(currentArtifactDir , hash.value+".jar") //TODO: need a smarter way to store artifacts (imagine 50K jars in one dir!)
      })
    }
    val (existing, nonExisting) = hashFiles.partition{ case (hash, coords, locations, file) => 
      file.exists && Hash.calculate(file) == hash
    }
    for {
      existingFiles <- EitherUtils.reduce[String, File](existing.map{ case (_,_, _, file ) => Right(file) }).right
      downloadedFiles <- EitherUtils.reduce[String, File](adept.download.Download(nonExisting, timeout)).right
    } yield {
      existingFiles ++ downloadedFiles
    }
  }
  
  def prune(modules: Seq[Module]): Seq[Module] = {
    Prune(modules)
  }
}

class Adept private[adept](val dir: File, val name: String) {
  protected val logger = LoggerFactory.getLogger(this.getClass)
  
  override def toString = {
    "Adept("+name+","+dir.getAbsolutePath+","+ lastCommit+")"
  }
  
  private lazy val git = Git.open(dir)
  
  /* add module to adept. return right with file containing module, left on file that could not be created*/
  def add(module: Module): Either[File, File] = {
    Add(dir, module)
  }

  
  def findModule(coords: Coordinates, hash: Option[Hash]): Option[Module] = {
    val file = new File(Add.getModuleDir(dir, coords), Add.modulesFilename)
    if (file.exists && file.isFile) {
      import org.json4s.native.JsonMethods._
      val modules = Module.read(parse(file))
      hash.map{ hash =>
        val filtered = modules.filter(_.artifact.hash == hash)
        assert(filtered.size < 2, "found more than 1 module with hash: " + hash + " in " + file + " found: " + filtered)
        filtered.headOption
      }.getOrElse{
        if (modules.size > 1) throw new Exception("found more than 1 module: " + hash + " in " + file + " found: " + modules) //TODO: either instead of option as return type and return all the failed modules
        modules.headOption
      }
    } else {
      None
    }
  }
  
  def dependencies(module: Module): Set[Module] = {
    Set(module) ++ module.dependencies.par.flatMap{ case Dependency(coords, hash)  => //TODO: check if par gives us anything!
      findModule(coords, Some(hash)).toSet.flatMap{ m: Module =>
        dependencies(m)
      }
    }
  }

  def repo = git.getRepository()

  def lastCommit(allCoords: Set[Coordinates]): Option[Hash] = {
    val paths = allCoords.map{ coords =>
      new File(Add.getModuleDir(dir, coords), Add.modulesFilename)
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
  
  def lastCommit = {
    val hash = try {
      val logIt = git.log()
                     .call()
                     .iterator()
      logIt.next.getName
    } catch {
      case e: org.eclipse.jgit.api.errors.NoHeadException => "EMPTY"
    }
    Hash(hash)
  }
  
  lazy val branchName = "master"
  
  def isLocal: Boolean = {
    try {
      repo.getConfig().getString(
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