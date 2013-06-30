package adept.core

import java.io._
import org.eclipse.jgit.api.Git
import org.eclipse.jgit.api.errors.TransportException
import adept.core.operations._
import adept.core.models._
import akka.util.FiniteDuration
import adept.utils._
import org.eclipse.jgit.lib._
import org.eclipse.jgit.transport._
import com.jcraft.jsch.JSch

object Adept {
  def dir(baseDir: File, name: String) = new File(baseDir, name)

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
        val currentArtifactDir = ModuleFiles.getModuleDir(artifactDir, coords)
        ModuleFiles.createDir(currentArtifactDir)
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
 
  
  /*TODO: replace with Tree builder
  def resolveConflicts(modules: Seq[Module]): Seq[Module] = {
    //ConflictResolver.evictConflicts(modules)
    //TODO:
    null
  }
   
   
  def resolveArtifacts(artifacts: Set[Artifact], configurations: Set[Configuration], confsExpr: String): Set[Artifact] = {
    //Resolve.modules(dependencies, confsExpr, findModule)._1
    
    ConfigurationResolver.resolve(configurations, confsExpr) match {
      case Right(foundConfs) =>
        val (allArtifacts, evicted) = Resolver.matchingArtifacts(artifacts, foundConfs) //TODO: handle evicted
        allArtifacts
      case Left(_) => Set.empty
    }
    
  }
  
  
  def resolveConfigurations(confsExpr: String, configurations: Set[Configuration]): Set[Configuration] ={
    ConfigurationResolver.resolve(configurations, confsExpr) match {
      case Right(confs) => confs
      case Left(_) => Set.empty 
    }
  }
  
  def resolveDependencyConfigurations(confsExpr: String, rootConfigurations: Set[Configuration], configurations: Set[Configuration]): Set[Configuration] ={
    ConfigurationResolver.resolve(rootConfigurations, confsExpr, configurations)  match {
      case Right(confs) => confs
      case Left(_) => Set.empty 
    }
  }
  * 
  */
}

class Adept private[adept](val dir: File, val name: String) extends Logging {

  override def toString = {
    "Adept("+name+","+dir.getAbsolutePath+","+ lastCommit+")"
  }

  private lazy val git = Git.open(dir)

  /* add module to adept. return right with file containing module, left on file that could not be created*/
  def add(module: Module): Either[File, File] = {
    repo.Add(dir, module)
  }


  def findModule(coords: Coordinates, hash: Option[Hash] = None): Option[Module] = {
    val file = new File(ModuleFiles.getModuleDir(dir, coords), ModuleFiles.modulesFilename)


    if (file.exists && file.isFile) {
      import org.json4s.native.JsonMethods._
      val maybeModules = Module.readSameCoordinates(parse(file))
      maybeModules.fold(
          error => throw new Exception(error),
          modules => hash.map{ hash =>
            val filtered = modules.filter(_.hash == hash)
            assert(filtered.size < 2, "found more than 1 module with hash: " + hash + " in " + file + " found: " + filtered)
            filtered.headOption
          }.getOrElse{
            if (modules.size > 1) throw new Exception("found more than 1 module: " + hash + " in " + file + " found: " + modules) //TODO: either instead of option as return type and return all the failed modules
            modules.headOption
          }
      )
    } else {
      None
    }
  }

  /* TODO: replace with Treebuilder
  def resolveModules(dependencies: Set[Dependency], configurations: Set[Configuration], confsExpr: String, configurationMapping: String => String): Seq[(Module, Set[Configuration])] = {
    val rootModules = dependencies.flatMap{ d =>
      findModule(d.coords, Some(d.hash))
    }
    val allModules = rootModules.flatMap{ rootModule =>
      val matchedConfs = Adept.resolveConfigurations(confsExpr, configurations)
      //Resolver.matchingModules(dependencies, matchedConfs, configurationMapping, findModule) //TODO: handle evicted
      Seq.empty//FIXME
    }
    allModules.toSeq
  }
  * 
  */

  def dependencies(module: Module): Set[Module] = {
    Set(module) ++ module.dependencies.par.flatMap{ case Dependency(coords, hash, _, _, _)  => //TODO: check if par gives us anything!
      findModule(coords, Some(hash)).toSet.flatMap{ m: Module =>
        dependencies(m)
      }
    }
  }

  private def gitRepo = git.getRepository()

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
