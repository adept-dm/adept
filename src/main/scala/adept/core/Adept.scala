package adept.core

import java.io.{File => jFile}
import slick.session.Database

//database
import db.driver.simple._

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
  
  def list(repoName: String)(implicit database: Database): Either[String, Seq[(Module, Repository)]]= {
    database.withSession{
      import slick.session.Database.threadLocalSession
      val foundModules = existingModules(repoName).list.map{ t => 
        val (m, r) = Modules.fromRow(t)
        (m, r)
      }
      Right(foundModules)
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

  def changes(repoName: String, from: Int = -1, index: Int = 0, max: Int = Integer.MAX_VALUE)(implicit database: Database): Either[String, Seq[ChangeSet]] ={
    import Database.threadLocalSession

    database.withTransaction{
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
      Right(changes.map{ case (repo, moduleChanges) => ChangeSet(repo, moduleChanges) })
    }
  }
  
  def pull(repoName: String)(implicit database: Database): Either[String, Repository] = {
    Left("not implemented")
  }
  
  def init(repoName: String)(implicit database: Database): Either[String, String]= {
    import Database.threadLocalSession

    database.withTransaction{
      if (db.checkExistence(implicitly[Session])) {
        val repository = Query(RepositoryVersions).filter(_.name === repoName).firstOption
        if (repository.isDefined) {
          Left(s"repository $repoName is already defined")
        } else {
          RepositoryVersions.insert(repoName, 0, true, false)
          Right(s"Initialized adept repository $repoName ")
        }
      } else {
        db.allDDLs.create
        RepositoryVersions.insert(repoName, 0, true, false)
        Right(s"Created new adept repository $repoName")
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
  def diff(repoName: String, from: Option[Int] = None, to: Option[Int] = None)(implicit database: Database): Either[String, Seq[Diff]] = {
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
     
      
      
      
     Right(Seq.empty)
    }
     */
    Left("not implemented")
  }
  
  private def currentVersion(repoName: String)(implicit session: Session): Option[Int] = { 
    val currentRepo = Query(RepositoryVersions)
        .filter(r => r.name === repoName)
    val active = currentRepo.filter(_.active).list
    if (active.length == 1) {
      currentRepo.filter(_.active).map(_.version).firstOption
    } else if (active.length > 1) {
      throw new Exception(s"FATAL: found more than one active repo with name: $repoName. (found: $active)")
    } else { //active.length < 1
      val last = Query(Query(RepositoryVersions)
        .filter(r => r.name === repoName)
        .map(_.version).max).firstOption.flatten
      val currentVersion = last.map(_ + 1)
      currentVersion.map { v =>
        RepositoryVersions.insert(repoName, v, true, false)
        v
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
    
    override def hashFoundInActive(activeModule: Module, activeRepoVersion: Int)(implicit session: Session): Either[String, (Module, Repository)] = {
      val existingModulesQ = Query(Modules)
        .filter( m => m.hash === hash.value && m.repoVersion === activeRepoVersion && m.deleted =!= true)
      val existingModules = onlyOption(existingModulesQ)
      existingModules.map{ _ =>
        existingModulesQ.map(_.deleted).update(true)
        Right(activeModule -> Repository(repoName, activeRepoVersion))
      }.getOrElse{
        Left(s"could not remove $hash because it has already been removed in $activeRepoVersion")
      }
    }
    
    override def hashWasRemoved(lastModule: Module, previousRepo: Int)(implicit session: Session): Either[String, (Module, Repository)] = {
      Left(s"could not remove $hash because it has already been removed in $previousRepo")
    }
    
    override def hashExists(lastModule: Module, previousRepo: Int)(implicit session: Session): Either[String, (Module, Repository)] = {
      currentVersion(repoName).map{ currentRepo =>
        Modules.insert( Modules.toRow(lastModule, repoName, currentRepo, true)  )
        Right(lastModule -> Repository(repoName, previousRepo))
      }.toRight(s"could not find current version or stage a new version in $repoName (tried to remove $hash)").joinRight
    }
    
    override def noHashFound(implicit session: Session): Either[String, (Module, Repository)] = {
      Left(s"could not remove $hash because it does not exist")
    }
  }
  
  /** set the module with hash in a repository  */
  def set(repoName: String, newModule: Module)(implicit database: Database) = {
    change(new SetExecutor(newModule, repoName), repoName, newModule.hash)
  }
  
  class SetExecutor(newModule: Module, repoName: String) extends Operation {

    private def newVersion(module: Module)(implicit session: Session): Either[String, (Module, Repository)] = {
      currentVersion(repoName).map{ currentRepo =>
        Modules.insert(Modules.toRow(module, repoName, currentRepo, deleted = false))
        Right(module -> Repository(repoName, currentRepo))
      }.toRight(s"could not find current version or stage a new version in $repoName (tried to set $newModule)").joinRight
    } 
    
    override def hashFoundInActive(activeModule: Module, activeRepoVersion: Int)(implicit session: Session): Either[String, (Module, Repository)] = {
      if (activeModule.hash != newModule.hash) throw new Exception(s"FATAL: for $repoName believed active ($activeModule in $activeRepoVersion) has the same hash as new module ($newModule)")
       val thisModuleQ = Query(Modules)
        .filter( m => m.hash === newModule.hash.value && m.repoVersion === activeRepoVersion )
        thisModuleQ
          .update(Modules.toRow(newModule, repoName, activeRepoVersion, deleted = false))
        Right(activeModule -> Repository(repoName, activeRepoVersion))
    }
    
    override def hashWasRemoved(lastModule: Module, previousRepo: Int)(implicit session: Session): Either[String, (Module, Repository)] = {
      newVersion(newModule)
    }
    
    override def hashExists(lastModule: Module, previousRepo: Int)(implicit session: Session): Either[String, (Module, Repository)] = {
      if (lastModule == newModule) Right(lastModule -> Repository(repoName, previousRepo))
      else newVersion(newModule)
    }
    
    override def noHashFound(implicit session: Session): Either[String, (Module, Repository)] = {
      newVersion(newModule)
    }
  }
  
  /** adds a module to a repository  */
  def add(repoName: String, module: Module)(implicit database: Database) = {
    change(new AddExecutor(module, repoName), repoName, module.hash)
  }

  class AddExecutor(module: Module, repoName: String) extends Operation {

    override def hashFoundInActive(activeModule: Module, activeRepoVersion: Int)(implicit session: Session): Either[String, (Module, Repository)] = {
      val thisModuleQ = Query(Modules)
        .filter( m => m.hash === module.hash.value && m.repoVersion === activeRepoVersion && m.repoName === repoName)
      thisModuleQ.firstOption.flatMap{ moduleRow =>
        val deleted = moduleRow._11
        val (module, repo) = Modules.fromRow(moduleRow)
        if (deleted) {
          thisModuleQ.update(Modules.toRow(module, repoName, activeRepoVersion, deleted = false))
          Some(Right(module -> Repository(repoName, activeRepoVersion)))
        } else if (module == activeModule)
          Some(Right(module -> repo))
        else None
      }.getOrElse {
        Left(s"cannot add module $module to $activeRepoVersion because $activeModule already exists")
      }
    }
    
    override def hashWasRemoved(lastModule: Module, previousRepo: Int)(implicit session: Session): Either[String, (Module, Repository)] = {
      currentVersion(repoName).map{ currentVersion => 
        Modules.insert(Modules.toRow(module, repoName, currentVersion, deleted = false))
        Right(module -> Repository(repoName, currentVersion))
      }.toRight(s"could not stage a new version or find the existing in $repoName (adding $module after $lastModule was removed in $previousRepo)").joinRight
    }
    
    
    override def hashExists(lastModule: Module, previousRepo: Int)(implicit session: Session): Either[String, (Module, Repository)] = {
      if (module == lastModule) {
        Right(module -> Repository(repoName, previousRepo))
      } else {
        Left(s"cannot add module $module because $lastModule already exists in $repoName for $previousRepo")
      }
    }
    
    override def noHashFound(implicit session: Session): Either[String, (Module, Repository)] = {
      currentVersion(repoName).map{ currentRepo =>
        Modules.insert(Modules.toRow(module, repoName, currentRepo, deleted = false))
        Right(module -> Repository(repoName, currentRepo))
      }.toRight(s"could not find current version or stage a new version in $repoName (tried to add $module)" ).joinRight
    }
  }

  
  trait Operation {
    /** a module with this hash has been found with an active version */ 
    def hashFoundInActive(activeModule: Module, activeRepoVersion: Int)(implicit session: Session): Either[String, (Module, Repository)]
    
    /** this module was removed in a previous version */ 
    def hashWasRemoved(lastModule: Module, previousRepo: Int)(implicit session: Session): Either[String, (Module, Repository)]

    /** this module was updated in a previous version */ 
    def hashExists(lastModule: Module, previousRepo: Int)(implicit session: Session): Either[String, (Module, Repository)]

    /** this hash does not exist in this repository */ 
    def noHashFound(implicit session: Session): Either[String, (Module, Repository)]
  }
  
  private def onlyOption[A, B](q: Query[A, B])(implicit session: Session) = {
    val all = q.list
    if (all.length > 1) throw new Exception(s"FATAL: expected to find maximum one, but found: $all")
    all.headOption
  }
  
  private def change(execute: Operation, repoName: String, hash: Hash)
    (implicit database: Database): Either[String, (Module, Repository)] = {
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
      }.joinRight
    }
  }
  
  /** checks if this is a repository where there is work */
  private def notDirty[A](repoName: String)(block: => A): Either[String, A] = {
    import Database.threadLocalSession
    val dirty = Query(Query(RepositoryVersions).filter(r => r.name === repoName && r.stashed).exists)
      .firstOption.getOrElse(true)
    if (dirty) Left(s"cannot use repository $repoName, because it contains stashed versions and is considered dirty")
    else Right(block)
  }
  
  
  private def activeModulesForRepo(repo: Repository)(implicit session: Session): List[(Module, Boolean)] = {
    val modules = for {
      m <- Modules
      if m.repoName === repo.name &&
        (m.repoVersion === repo.version)
    } yield m
    modules.list.map( t => Modules.fromRow(t) match { 
      case (module, repo) => module -> t._11
    })
  }
  
  private def verifyArtifacts(module: Module, doCheck: Boolean): Either[String, Unit] = {
    Left("not implemented") //TODO
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
  
  private def verifyActiveDeps(moduleDeleted: (Module, Boolean), repoName: String, version: Int)(implicit session: Session): Either[String, Unit] = {
    val (module, deleted) = moduleDeleted
    if (deleted) {
      val dependingOn = modulesDependingOn(module, repoName).map(_.hash).list
      if (dependingOn.size > 0)
        Left(s"cannot delete $module because these modules depend on it: ${dependingOn.mkString(",")}")
      else Right()
    } else {
      val missingHashes = module.deps.filter{ depHash =>
        Query(activeModulesFor(depHash.value, repoName, false).exists).firstOption.getOrElse(false) == false
      }
      
      if (missingHashes.size > 0) {
        Left(s"missing dependencies for $module in $repoName version $version. These hashes were not found: ${missingHashes.mkString(",")}")
      } else Right()
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
  def commit(repoName: String, checkArtifacts: Boolean = true)(implicit database: Database): Either[String, Repository] = {
    import Database.threadLocalSession
    database.withTransaction{
      notDirty(repoName){
        val activeQ = Query(RepositoryVersions)
          .filter(r => r.name === repoName).filter(_.active)
        val active = activeQ.list
        if (active.length == 1) {
          val (name, activeVersion, _, _) = active.headOption.get
          val activeModules = activeModulesForRepo(Repository(name, activeVersion))
          val modulesCheck = activeModules.foldLeft(Right(): Either[String, Unit])( (current, thisModuleDeleted) =>
            for {
              c <- current.right
              //TODO:_ <- verifyArtifacts(thisModule, checkArtifacts).right
              _ <- verifyActiveDeps(thisModuleDeleted, name, activeVersion).right
            } yield ()
          ).right.map( _ => Repository(name, activeVersion))
          if (modulesCheck.isRight) {
            activeQ.update(name, activeVersion, false, false)
          }
          modulesCheck
        } else if (active.length > 1) {
          throw new Exception(s"FATAL: found more than one active repo with name: $repoName. (found: $active)")
        } else {
          Left("nothing to commit")
        }
      }.joinRight
    }
  }
  
  /** returns the module and all its dependencies matching the coordinates and metadata */
  def describe(coords: Coordinates, meta: Metadata)(implicit database: Database): Either[String, (Module, Seq[Module])] = {
    
    null
  }
  
}
