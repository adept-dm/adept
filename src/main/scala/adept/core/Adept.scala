package adept.core

import java.io.{File => jFile}
import slick.session.Database
import util._
import db.driver.simple._
import scala.collection.parallel.ParSeq

object Adept {
  
  //TODO: create QueryTemplates
  private def lastVersionFor(repoName: String, hash: Column[String], active: Boolean) = 
    repositoriesFor(repoName, hash).filter(_.active === active).map(_.version).max

  private def lastVersionFor(repoName: String, hash: Column[String]) = 
    repositoriesFor(repoName, hash).map(_.version).max

  private def repositoriesFor(repoName: String, hash: Column[String]) = 
   for {
     r <- RepositoryVersions if r.name === repoName
     m <- Modules
     if m.repoVersion === r.version && 
        m.repoName === r.name && 
        m.hash === hash
   } yield {
     r
   }
   
  private def existingModules(repoName: String) = 
    for{
      m <- Modules 
      if m.repoName === repoName &&
         m.deleted === false &&
         //only last ones:
         m.repoVersion === lastVersionFor(repoName, m.hash, false)
    } yield {
      m
    }
  
  def list(repoName: String)(implicit database: Database): Try[Seq[(Module, VersionId)]] = {
    database.withSession{ implicit s: Session =>
      val foundModules = existingModules(repoName).list.map{ t => 
        val (m, r) = Modules.fromRow(t)
        (m, r)
      }
      Success(foundModules)
    }
  }
  
  def repoList(implicit database: Database): Seq[(Repository, Option[Int])]= {
    /*
    database.withSession{
      val q = for {
        (repo, staged) <- RepositoryMetadata leftJoin StagedRepositories on ( (r, s) => r.name === s.name)
      } yield {
        repo -> staged.version.?
      }
      val all = q.list.map{ case ((name, version), staged) => Repository(name, version) -> staged}
      all.groupBy(_._1.name).map{ case (name, all) => //TODO: do this in DB?
        all.maxBy(_._1.version)
      }.toSeq
    }*/
    null
  }

  def changes(repoName: String, from: Int = -1, index: Int = 0, max: Int = Integer.MAX_VALUE)(implicit database: Database): Try[Seq[ChangeSet]] ={
    database.withTransaction{ implicit session: Session =>
      val changesQ = for {
        r <- RepositoryVersions if r.version >= from  
        m <- Modules
        if m.repoVersion === r.version &&
           m.repoName === r.name
      } yield {
        r -> m
      }
      val changes = changesQ.drop(index).take(max).list.groupBy(_._1).map{ case (repo, repoModuleRows) =>
        RepositoryVersions.fromRow(repo)._1 -> {
          repoModuleRows.map{ case (repo, moduleRow) =>
            Change(Modules.fromRow(moduleRow)._1, moduleRow._11) //FIXME: _11 is deleted but this will end up hurting you
          }
        }
      }.toSeq
      Success(changes.map{ case (repo, moduleChanges) => ChangeSet(repo, moduleChanges) })
    }
  }
  
  def applyChangeSet(changeSet: ChangeSet)(implicit database: Database): Try[Repository] = {
    val repo = changeSet.repo
  
    database.withTransaction{ implicit s: Session =>
      changeSet.moduleChanges.par
        .map(c => Modules.toRow(c.module, repo.id.name, repo.id.version, c.deleted))
        .foreach(Modules.insert)
    }
    Success(repo)
  }
  
  //reduce Try; TODO: move
  private def reduce[B](s: ParSeq[Try[B]]): Try[Seq[B]] =
      s.foldLeft(Success(Nil): Try[List[B]]) {
        (acc, e) => for (xs <- acc; x <- e) yield x :: xs
      }.map(_.reverse)

  def merge(changeSets: ParSeq[ChangeSet])(implicit database: Database): Try[Repository] = {
    //val repositories = database.withTransaction{  implicit s: Session =>
    //  changeSets.
    //}
    //repositories.map(_.maxBy(_.version))
    null
  }
  
  def init(repoName: String)(implicit database: Database): Try[String]= {
    database.withTransaction{ implicit s: Session =>
      if (db.checkExistence(implicitly[Session])) {
        val repository = Query(RepositoryVersions).filter(_.name === repoName).firstOption
        if (repository.isDefined) {
          Failure(new Exception(s"repository $repoName is already defined"))
        } else {
          RepositoryVersions.insert(repoName, 0, None, true, false)
          Success(s"Initialized adept repository $repoName ")
        }
      } else {
        db.allDDLs.create
        RepositoryVersions.insert(repoName, 0, None, true, false)
        Success(s"Created new adept repository $repoName")
      }
    }
  }
  
  sealed trait Diff {
    val repo: Repository
  }
  
  case class Updated(original: Module, originalRepo: Repository, updated: Module, override val repo: Repository) extends Diff {
    override def toString = {
      s"""updated from: $original ($originalRepo)\n"""+
      s"""updated to  : $updated ($repo)"""
    }
  }
  
  case class Deleted(module: Module, override val repo: Repository) extends Diff {
    override def toString = {
      s"""deleted     : $module ($repo)"""
    }
  }
  
  case class Inserted(module: Module, override val repo: Repository) extends Diff {
    override def toString = {
      s"""added       : $module ($repo)"""
    }
  }
  
  /** diffs  */
  def diff(repoName: String, from: Option[Int] = None, to: Option[Int] = None)(implicit database: Database): Try[Seq[Diff]] = {
    /*TODO
    import Database.threadLocalSession
    database.withTransaction{
      val inserts = (for {
        m1 <- Modules
        m2 <- Modules
        if m1.repoName === repoName && m1.repoVersion >= from.getOrElse(-1) && m1.repoVersion <= to.getOrElse(Integer.MAX_VALUE) && 
           m1.deleted === false
        if m2.repoName === repoName && m2.hash === m1.hash && m2.repoVersion === maxForHash(m1.hash, m1.repoVersion)   
      } yield {
        m1
      }).list
        .map(t => Modules.fromRow(t) match { case (m, r) => Inserted(m, r) })
      
        
      val deletes = (for {
        m <- Modules
        if m.repoVersion >= from.getOrElse(-1) && m.repoVersion <= to.getOrElse(Integer.MAX_VALUE) && 
           m.deleted === true &&
           m.repoName === repoName
      } yield {
        m
      }).list
        .map(t => Modules.fromRow(t) match { case (m, r) => Deleted(m, r) })
     
      
      
      
     Success(Seq.empty)
    }
     */
    Failure(new Exception("not implemented"))
  }
  
  private def currentVersion(repoName: String)(implicit session: Session): Try[Int] = { 
    val currentRepo = Query(RepositoryVersions)
        .filter(r => r.name === repoName)
    val active = currentRepo.filter(_.active).list
    if (active.length == 1) {
      currentRepo.filter(_.active).map(_.version).firstOption.map( v => Try(v) ).getOrElse{
        Failure(new Exception(s"could not find active version in $repoName"))
      }
    } else if (active.length > 1) {
      throw new Exception(s"FATAL: found more than one active repo with name: $repoName. (found: $active)")
    } else { //active.length < 1
      val last = Query(Query(RepositoryVersions)
        .filter(r => r.name === repoName)
        .map(_.version).max).firstOption.flatten
      val currentVersion = last.map(_ + 1)
      currentVersion.map { v =>
        RepositoryVersions.insert(repoName, v, None, true, false)
        Try(v)
      }.getOrElse{
        Failure(new Exception(s"could not find or create a new version in $repoName"))
      }
    }
  }
  
  private def isDeleted(module: Module, repoName: String, repoVersion: Int)(implicit session: Session)= {
    Query(Query(Modules)
      .filter(m => 
        m.repoName === repoName
          && m.repoVersion <= repoVersion
          && m.repoVersion > Query(Modules).filter{ latest => 
            latest.repoName === repoName &&
            latest.hash === module.hash.value &&
            latest.repoVersion <= repoVersion &&
            latest.deleted === true
          }.map(_.repoVersion).max
          && m.hash === module.hash.value 
          && m.deleted === true
     ).exists).firstOption.getOrElse(false)
  }
  
  /** removes the module with hash from a repository  */
  def remove(repoName: String, hash: Hash)(implicit database: Database) = {
    change(new RemoveExecutor(hash, repoName), repoName, hash)
  }
  
  class RemoveExecutor(hash: Hash, repoName: String) extends Operation {
    
    override def hashFoundInActive(activeModule: Module, activeRepoVersion: Int)(implicit session: Session): Try[(Module, VersionId)] = {
      val existingModulesQ = Query(Modules)
        .filter( m => m.hash === hash.value && m.repoVersion === activeRepoVersion && m.deleted =!= true)
      val existingModules = onlyOption(existingModulesQ)
      existingModules.map{ _ =>
        existingModulesQ.map(_.deleted).update(true)
        Success(activeModule -> VersionId(repoName, activeRepoVersion))
      }.getOrElse{
        Failure(new Exception(s"could not remove $hash because it has already been removed in $activeRepoVersion"))
      }
    }
    
    override def hashWasRemoved(lastModule: Module, previousRepo: Int)(implicit session: Session): Try[(Module, VersionId)] = {
      Failure(new Exception(s"could not remove $hash because it has already been removed in $previousRepo"))
    }
    
    override def hashExists(lastModule: Module, previousRepo: Int)(implicit session: Session): Try[(Module, VersionId)] = {
      currentVersion(repoName).flatMap{ currentRepo =>
        Modules.insert( Modules.toRow(lastModule, repoName, currentRepo, true)  )
        Success(lastModule -> VersionId(repoName, previousRepo))
      }
    }
    
    override def noHashFound(implicit session: Session): Try[(Module, VersionId)] = {
      Failure(new Exception(s"could not remove $hash because it does not exist"))
    }
  }
  
  /** set the module with hash in a repository  */
  def set(repoName: String, newModule: Module)(implicit database: Database) = {
    change(new SetExecutor(newModule, repoName), repoName, newModule.hash)
  }
  
  class SetExecutor(newModule: Module, repoName: String) extends Operation {

    private def newVersion(module: Module)(implicit session: Session): Try[(Module, VersionId)] = {
      currentVersion(repoName).flatMap{ currentRepo =>
        Modules.insert(Modules.toRow(module, repoName, currentRepo, deleted = false))
        Success(module -> VersionId(repoName, currentRepo))
      }
    } 
    
    override def hashFoundInActive(activeModule: Module, activeRepoVersion: Int)(implicit session: Session): Try[(Module, VersionId)] = {
      if (activeModule.hash != newModule.hash) throw new Exception(s"FATAL: for $repoName believed active ($activeModule in $activeRepoVersion) has the same hash as new module ($newModule)")
       val thisModuleQ = Query(Modules)
        .filter( m => m.hash === newModule.hash.value && m.repoVersion === activeRepoVersion )
        thisModuleQ
          .update(Modules.toRow(newModule, repoName, activeRepoVersion, deleted = false))
        Success(activeModule -> VersionId(repoName, activeRepoVersion))
    }
    
    override def hashWasRemoved(lastModule: Module, previousRepo: Int)(implicit session: Session): Try[(Module, VersionId)] = {
      newVersion(newModule)
    }
    
    override def hashExists(lastModule: Module, previousRepo: Int)(implicit session: Session): Try[(Module, VersionId)] = {
      if (lastModule == newModule) Success(lastModule -> VersionId(repoName, previousRepo))
      else newVersion(newModule)
    }
    
    override def noHashFound(implicit session: Session): Try[(Module, VersionId)] = {
      newVersion(newModule)
    }
  }
  
  /** adds a module to a repository  */
  def add(repoName: String, module: Module)(implicit database: Database) = {
    change(new AddExecutor(module, repoName), repoName, module.hash)
  }

  class AddExecutor(module: Module, repoName: String) extends Operation {

    override def hashFoundInActive(activeModule: Module, activeRepoVersion: Int)(implicit session: Session): Try[(Module, VersionId)] = {
      val thisModuleQ = Query(Modules)
        .filter( m => m.hash === module.hash.value && m.repoVersion === activeRepoVersion && m.repoName === repoName)
      thisModuleQ.firstOption.flatMap{ moduleRow =>
        val deleted = moduleRow._11
        val (module, repo) = Modules.fromRow(moduleRow)
        if (deleted) {
          thisModuleQ.update(Modules.toRow(module, repoName, activeRepoVersion, deleted = false))
          Some(Success(module -> VersionId(repoName, activeRepoVersion)))
        } else if (module == activeModule)
          Some(Success(module -> repo))
        else None
      }.getOrElse {
        Failure(new Exception(s"cannot add module $module to $activeRepoVersion because $activeModule already exists"))
      }
    }
    
    override def hashWasRemoved(lastModule: Module, previousRepo: Int)(implicit session: Session): Try[(Module, VersionId)] = {
      currentVersion(repoName).flatMap{ currentVersion => 
        Modules.insert(Modules.toRow(module, repoName, currentVersion, deleted = false))
        Success(module -> VersionId(repoName, currentVersion))
      }
    }
    
    
    override def hashExists(lastModule: Module, previousRepo: Int)(implicit session: Session): Try[(Module, VersionId)] = {
      if (module == lastModule) {
        Success(module -> VersionId(repoName, previousRepo))
      } else {
        Failure(new Exception(s"cannot add module $module because $lastModule already exists in $repoName for $previousRepo"))
      }
    }
    
    override def noHashFound(implicit session: Session): Try[(Module, VersionId)] = {
      currentVersion(repoName).flatMap{ currentRepo =>
        Modules.insert(Modules.toRow(module, repoName, currentRepo, deleted = false))
        Success(module -> VersionId(repoName, currentRepo))
      }
    }
  }

  
  trait Operation {
    /** a module with this hash has been found with an active version */ 
    def hashFoundInActive(activeModule: Module, activeRepoVersion: Int)(implicit session: Session): Try[(Module, VersionId)]
    
    /** this module was removed in a previous version */ 
    def hashWasRemoved(lastModule: Module, previousRepo: Int)(implicit session: Session): Try[(Module, VersionId)]

    /** this module was updated in a previous version */ 
    def hashExists(lastModule: Module, previousRepo: Int)(implicit session: Session): Try[(Module, VersionId)]

    /** this hash does not exist in this repository */ 
    def noHashFound(implicit session: Session): Try[(Module, VersionId)]
  }
  
  private def onlyOption[A, B](q: Query[A, B])(implicit session: Session) = {
    val all = q.list
    if (all.length > 1) throw new Exception(s"FATAL: expected to find maximum one, but found: $all")
    all.headOption
  }
  
  private def change(execute: Operation, repoName: String, hash: Hash)
    (implicit database: Database): Try[(Module, VersionId)] = {
    import Database.threadLocalSession
  
    database.withTransaction{
      notDirty(repoName){
      
        val thisModuleQ = Query(Modules).filter(m => m.hash === hash.value && m.repoName === repoName).sortBy(_.repoVersion.desc)
        val maybeFoundModuleRepo = thisModuleQ.take(1).firstOption.map(Modules.fromRow)
        val maybeFoundVersion = maybeFoundModuleRepo.map{ case (n, r) => r.version}
        maybeFoundVersion.map { foundVersion =>
          
          val maybeActiveRepoVersion = onlyOption(Query(RepositoryVersions).filter( r => r.name === repoName && r.version === foundVersion && r.active).map(_.version))
          
          maybeActiveRepoVersion.map{ activeRepoVersion => //active repository
            onlyOption(Query(Modules)
                .filter(m => m.repoName === repoName && 
                    m.hash === hash.value 
                    && m.repoVersion === activeRepoVersion))
                .map(Modules.fromRow)
                .map{ 
              case (activeModule, _)=>
              execute.hashFoundInActive(activeModule, activeRepoVersion)
            }.getOrElse{
              throw new Exception(s"FATAL: expected to find on module ${hash.value} in repo version $activeRepoVersion")
            }
            
          }.getOrElse { //no repository has been staged
            val lastModuleQ = for{
              m <- Modules 
              if m.repoName === repoName &&
                 m.hash === hash.value &&
                 m.repoVersion === lastVersionFor(repoName, m.hash, false)
            } yield {
              m
            }
            val ((lastModule, lastRepo), deleted) = onlyOption(lastModuleQ).map(t => Modules.fromRow(t) -> t._11).get
            if (deleted) {
              execute.hashWasRemoved(lastModule, lastRepo.version)
            } else {
              execute.hashExists(lastModule, lastRepo.version)
            }
          }
        }.getOrElse{
          execute.noHashFound
        }
      }.flatten
    }
  }
  
  /** checks if this is a repository where there is work */
  private def notDirty[A](repoName: String)(block: => A): Try[A] = {
    import Database.threadLocalSession
    val dirty = Query(Query(RepositoryVersions).filter(r => r.name === repoName && r.stashed).exists)
      .firstOption.getOrElse(true)
    if (dirty) Failure(new Exception(s"cannot use repository $repoName, because it contains stashed versions and is considered dirty"))
    else Success(block)
  }
  
  
  private def activeModulesForVersion(versionId: VersionId)(implicit session: Session): List[(Module, Boolean)] = {
    val modules = for {
      m <- Modules
      if m.repoName === versionId.name &&
        (m.repoVersion === versionId.version)
    } yield m
    modules.list.map( t => Modules.fromRow(t) match { 
      case (module, repo) => module -> t._11
    })
  }
  
  private def verifyArtifacts(module: Module, doCheck: Boolean): Try[Unit] = {
    Failure(new Exception("not implemented")) //TODO
  }
  
  private def activeModulesFor(hash: String, repoName: String, deleted: Boolean) = (for{
    r <- RepositoryVersions
    if r.name === repoName && r.active 
    m <- Modules 
    if m.repoName === repoName &&
       m.hash === hash &&
       m.deleted === deleted &&
       m.repoVersion === r.version
  } yield {
    m
  })
  
  private def verifyActiveDeps(moduleDeleted: (Module, Boolean), repoName: String, version: Int)(implicit session: Session): Try[Unit] = {
    val (module, deleted) = moduleDeleted
    if (deleted) {
      val dependingOn = modulesDependingOn(module, repoName).map(_.hash).list
      if (dependingOn.size > 0)
        Failure(new Exception(s"cannot delete $module because these modules depend on it: ${dependingOn.mkString(",")}"))
      else Success()
    } else {
      val missingHashes = module.deps.filter{ depHash =>
        Query(activeModulesFor(depHash.value, repoName, false).exists).firstOption.getOrElse(false) == false
      }
      
      if (missingHashes.size > 0) {
        Failure(new Exception(s"missing dependencies for $module in $repoName version $version. These hashes were not found: ${missingHashes.mkString(",")}"))
      } else Success()
    }
  }
  
  private def modulesDependingOn(module: Module, repoName: String) = for{
    m <- Modules 
    if m.repoName === repoName &&
       m.deleted === false &&
       m.repoVersion === lastVersionFor(repoName, m.hash) &&
       m.childHashes.like("%"+module.hash.value+"%")
  } yield {
    m
  }
  
  /** creates a new repository version from the currently staged repository */
  def commit(repoName: String, checkArtifacts: Boolean = true)(implicit database: Database): Try[VersionId] = {
    import Database.threadLocalSession
    database.withTransaction{
      notDirty(repoName){
        val activeQ = Query(RepositoryVersions)
          .filter(r => r.name === repoName).filter(_.active)
        val active = activeQ.list
        if (active.length == 1) {
          val (name, activeVersion, _, _, _) = active.headOption.get
          val activeModules = activeModulesForVersion(VersionId(name, activeVersion))
          val modulesCheck = activeModules.foldLeft(Try(): Try[Unit])( (current, thisModuleDeleted) =>
            for {
              c <- current
              //TODO:_ <- verifyArtifacts(thisModule, checkArtifacts)
              _ <- verifyActiveDeps(thisModuleDeleted, name, activeVersion)
            } yield {
              
            }
          ).map( _ => VersionId(name, activeVersion))
          
          if (modulesCheck.isSuccess) {
            val repoHash = Hash.calculate(activeModules.map{ case (module, _) => module.hash })
            activeQ.update(name, activeVersion, Some(repoHash.value), false, false)
          }
          modulesCheck
        } else if (active.length > 1) {
          throw new Exception(s"FATAL: found more than one active repo with name: $repoName. (found: $active)")
        } else {
          Failure(new Exception("nothing to commit"))
        }
      }.flatten
    }
  }
  
  /** returns the module and all its dependencies matching the coordinates and metadata */
  def describe(coords: Coordinates, meta: Metadata)(implicit database: Database): Try[(Module, Seq[Module])] = {
    
    null
  }
  
}
