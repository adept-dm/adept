package adept.repository

import java.io.File
import net.sf.ehcache.CacheManager
import org.eclipse.jgit.api.Git

//TODO: change name! RepositoryHandle perhaps?
class RepositoryRefs private[repository] (private[repository] val result: Either[Set[String], Set[LocalGitRepository]]) {
  def isFailure(): Boolean = result.isLeft
  def getErrorMessages(): Set[String] = result.fold(errors => errors, repos => Set.empty)
  def getDirs(): Set[File] = result.fold(errors => Set.empty, repos => repos.map(_.repoDir))

  def add(refs: RepositoryRefs): RepositoryRefs = {
    (this.result, refs.result) match {
      case (Left(msgs1), Left(msgs2)) => new RepositoryRefs(Left(msgs1 ++ msgs2))
      case (Right(_), Left(msgs2)) => new RepositoryRefs(Left(msgs2))
      case (Left(msgs1), Right(_)) => new RepositoryRefs(Left(msgs1))
      case (Right(names1), Right(names2)) => new RepositoryRefs(Right(names1 ++ names2))
    }
  }

  def ++(refs: RepositoryRefs): RepositoryRefs = add(refs)
}

class AdeptOpenRepositoryException(baseDir: File, errors: Set[String]) extends Exception(s"Could not open repositories in $baseDir. Got errors: ${errors.mkString("\n")}")

/**
 *
 * Usage:
 * {{{
 *
 * val repos = AdeptRepositoryManager.getExisting(dir) ++
 *    AdeptRepositoryManager.init(dir, name) ++
 *    AdeptRepositoryManager.getExisting(dir, name, commit)
 *
 * val manager = AdeptRepositoryManager.open(dir, repos, cacheManager)
 * }}}
 */
object AdeptRepositoryManager {
  val DefaultCacheManager = CacheManager.create()

  def exists(baseDir: File, name: String): Boolean = {
    val repoDir = Repository.getRepoDir(baseDir, name)
    repoDir.isDirectory()
  }
  
  def getExisting(baseDir: File): RepositoryRefs = {
    val result = if (baseDir.isDirectory) {
      val reposDir = Repository.getReposDir(baseDir)
      val names = reposDir.listFiles.filter(_.isDirectory).map(_.getName).toSet
      Right(names.map(new LocalGitRepository(baseDir, _, Commit.Head)))
    } else Left(Set(s"Could not find a directory named: '$baseDir'"))
    new RepositoryRefs(result)
  }

  def getExisting(baseDir: File, name: String, commit: Commit = Commit.Head): RepositoryRefs = {
    val result: Either[Set[String], Set[LocalGitRepository]] = if (baseDir.isDirectory) {
      val repoDir = Repository.getRepoDir(baseDir, name)
      if (!repoDir.isDirectory()) Left(Set(s"Could not find a directory for repository: '$repoDir'"))
      else { //TODO: check if commit exists!
        Right(Set(new LocalGitRepository(baseDir, name, commit)))
      }
    } else Left(Set(s"Could not find a directory named: '$baseDir'"))
    new RepositoryRefs(result)
  }

  def init(baseDir: File, name: String): RepositoryRefs = {
    val repoDir = Repository.getRepoDir(baseDir, name)
    val result = if (repoDir.isDirectory) Left(Set(s"Cannot initialise '$repoDir', because directory already exists"))
    else {
      if (repoDir.mkdirs()) {
        Git.init()
          .setDirectory(repoDir)
          .call()

        //We need a commit in order to be able to reference it from the very start:
        val gitRepo = Git.open(repoDir)
        
        gitRepo.commit()
          .setMessage("Initialised " + name)
          .call()
        gitRepo.tag().setName(Repository.InitTag).call()
          
        Right(Set(new LocalGitRepository(baseDir, name, Commit.Head)))
      } else {
        Left(Set(s"Could not make directory '$repoDir'"))
      }
    }
    new RepositoryRefs(result)
  }

  def open(baseDir: File, repoRefs: RepositoryRefs, cacheManager: CacheManager = DefaultCacheManager): GitRepositoryEngine = {
    repoRefs.result match {
      case Left(msgs) => throw new AdeptOpenRepositoryException(baseDir, repoRefs.getErrorMessages)
      case Right(repos) => new GitRepositoryEngine(baseDir, repos, cacheManager)
    }
  }

  //TODOs: clone, push, pull
}
