package adept.core

import java.io.{File => jFile}
import slick.session.Database

//database
import db.driver.simple._

object Adept {
  def list(name: String)(implicit database: Database): Either[String, Seq[(Module, Repository)]]= {
    
    null
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

  def init(name: String)(implicit database: Database): Either[String, String]= {
    import Database.threadLocalSession

    database.withTransaction{
      if (db.checkExistence(implicitly[Session])) {
        val repository = Query(RepositoryVersions).filter(_.name === name).firstOption
        if (repository.isDefined) {
          Left(s"repository $name is already defined")
        } else {
          RepositoryVersions.insert(name, 0, true, false)
          Right(s"Initialized adept repository $name ")
        }
      } else {
        db.allDDLs.create
        RepositoryVersions.insert(name, 0, true, false)
        Right(s"Created new adept repository $name")
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
    import Database.threadLocalSession
    
    database.withTransaction{
      val inserts = (for {
        m <- Modules
        if m.repoVersion >= from.getOrElse(-1) && m.repoVersion <= to.getOrElse(Integer.MAX_VALUE) && 
           m.change.isNull &&
           m.repoName === repoName
      } yield {
        m
      }).list.map(t => Modules.fromRow(t)).map{ case (m, r) => Inserted(m, r) }
      
      val deletes = (for {
        m <- Modules
        if m.repoVersion >= from.getOrElse(-1) && m.repoVersion <= to.getOrElse(Integer.MAX_VALUE) && 
           m.change === Changes.Deleted &&
           m.repoName === repoName
      } yield {
        m
      }).list.map(t => Modules.fromRow(t)).map{ case (m, r) => Deleted(m, r) }
      
      val updates = (for {
        updatedModule <- Modules
        originalModule <- Modules
        if updatedModule.hash === originalModule.hash &&
           updatedModule.repoName === repoName &&
           originalModule.repoName === repoName &&
           updatedModule.repoVersion >= from.getOrElse(-1) && updatedModule.repoVersion <= to.getOrElse(Integer.MAX_VALUE) &&
           originalModule.repoVersion >= Query(originalModule.repoVersion).filter(_ < updatedModule.repoVersion).max && //original version is the highest one that is less then the updated version
           updatedModule.change === Changes.Updated && originalModule.change.isNull
      } yield { 
        originalModule -> updatedModule 
      }).list
        .map{ case (o, u) => 
          val (om, or) = Modules.fromRow(o) 
          val (um, ur) = Modules.fromRow(u)
          Updated(om, or, um, ur)
        } 
      val diffs = (inserts ++ deletes ++ updates).sortBy(_.repo.version)
      
      println( diffs.mkString("\n") )
      Right(diffs)
    }
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
      println("bumping version to " + currentVersion)
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
            (latest.change.isNull || latest.change === Changes.Updated)
          }.map(_.repoVersion).max
          && m.hash === module.hash.value 
          && m.change === Changes.Deleted
     ).exists).firstOption.getOrElse(false)
  }
  
  /** removes the module with hash from a repository  */
  def remove(repoName: String, hash: Hash)(implicit database: Database) = {
    change(new RemoveExecutor(hash, repoName), repoName, hash)
  }
  
  class RemoveExecutor(hash: Hash, repoName: String) extends Operation {
    
    override def hashFoundInActive(activeModule: Module, activeRepoVersion: Int)(implicit session: Session): Either[String, (Module, Repository)] = {
      Query(Modules)
        .filter( m => m.hash === hash.value && m.repoVersion === activeRepoVersion && m.change =!= Changes.Deleted)
        .delete
      Right(activeModule -> Repository(repoName, activeRepoVersion))
    }
    
    override def hashWasRemoved(lastModule: Module, previousRepo: Int)(implicit session: Session): Either[String, (Module, Repository)] = {
      Left(s"could not remove $hash because it has already been removed")
    }
    
    override def hashWasUpdated(lastModule: Module, previousRepo: Int)(implicit session: Session): Either[String, (Module, Repository)] = {
      currentVersion(repoName).map{ currentRepo =>
        Modules.insert( Modules.toRow(lastModule, repoName, currentRepo, Changes.Deleted)  )
        Right(lastModule -> Repository(repoName, previousRepo))
      }.toRight(s"could not find current version or stage a new version in $repoName (tried to remove $hash)").joinRight
    }
    
    override def hashWasAdded(lastModule: Module, previousRepo: Int)(implicit session: Session): Either[String, (Module, Repository)] = {
      hashWasUpdated(lastModule, previousRepo)
    }
    
    override def noHashFound(implicit session: Session): Either[String, (Module, Repository)] = {
      Left(s"could not remove $hash because it does not exist")
    }
  }
  
  /** updates the module with hash in a repository  */
  def update(repoName: String, hash: Hash, newModule: Module)(implicit database: Database) = {
    change(new UpdateExecutor(hash, newModule, repoName), repoName, hash)
  }
  
  class UpdateExecutor(hash: Hash, newModule: Module, repoName: String) extends Operation {

    override def hashFoundInActive(activeModule: Module, activeRepoVersion: Int)(implicit session: Session): Either[String, (Module, Repository)] = {
      val thisModuleQ = Query(Modules)
        .filter( m => m.hash === hash.value && m.repoVersion === activeRepoVersion )
      val change = onlyOption(thisModuleQ.map(_.change)).get
      if (change == Changes.Deleted) {
        Left(s"could not update $hash to $newModule because this hash was removed in $activeRepoVersion")
      } else {
        thisModuleQ
          .update(Modules.toRow(newModule, repoName, activeRepoVersion, change)) //insert is the same as adding when it is active
        Right(activeModule -> Repository(repoName, activeRepoVersion))
      }
    }
    
    override def hashWasRemoved(lastModule: Module, previousRepo: Int)(implicit session: Session): Either[String, (Module, Repository)] = {
      Left(s"could not update $hash to $newModule because this hash was removed in $previousRepo")
    }
    
    override def hashWasUpdated(lastModule: Module, previousRepo: Int)(implicit session: Session): Either[String, (Module, Repository)] = {
      if (lastModule == newModule) {
        Right(lastModule -> Repository(repoName, previousRepo))
      } else {
        currentVersion(repoName).map{ currentRepo =>
          Modules.insert( Modules.toRow(newModule, repoName, currentRepo, Changes.Updated)  )
          Right(lastModule -> Repository(repoName, currentRepo))
        }.toRight(s"could not find current version or stage a new version in $repoName (tried to update $hash)").joinRight
      }
    }
    
    override def hashWasAdded(lastModule: Module, previousRepo: Int)(implicit session: Session): Either[String, (Module, Repository)] = {
      hashWasUpdated(lastModule, previousRepo)
    }
    
    override def noHashFound(implicit session: Session): Either[String, (Module, Repository)] = {
      Left(s"could not update $hash to $newModule because there is no hash like this")
    }
  }
  
  /** adds a module to a repository  */
  def add(repoName: String, module: Module)(implicit database: Database) = {
    change(new AddExecutor(module, repoName), repoName, module.hash)
  }

  class AddExecutor(module: Module, repoName: String) extends Operation {
  
    override def hashFoundInActive(activeModule: Module, activeRepoVersion: Int)(implicit session: Session): Either[String, (Module, Repository)] = {
      val thisModuleQ = Query(Modules)
        .filter( m => m.hash === module.hash.value && m.repoVersion === activeRepoVersion )
      val change = onlyOption(thisModuleQ.map(_.change)).get
      println("______" + thisModuleQ.list)
      if (change != Changes.Deleted && module == activeModule) {
        Right(module -> Repository(repoName, activeRepoVersion))
      } else if (change == Changes.Deleted && module == activeModule) {
        thisModuleQ.delete
        Right(module -> Repository(repoName, activeRepoVersion))
      } else {
        Left(s"cannot add module $module to $activeRepoVersion because $activeModule already exists")
      }
    }
    override def hashWasRemoved(lastModule: Module, previousRepo: Int)(implicit session: Session): Either[String, (Module, Repository)] = {
      currentVersion(repoName).map{ currentVersion => 
        Modules.insert(Modules.toRow(module, repoName, currentVersion, Changes.Insert))
        Right(module -> Repository(repoName, currentVersion))
      }.toRight(s"could not stage a new version or find the existing in $repoName (adding $module after $lastModule was removed in $previousRepo)").joinRight
    }
    
    override def hashWasUpdated(lastModule: Module, previousRepo: Int)(implicit session: Session): Either[String, (Module, Repository)] = {
      if (module == lastModule) {
        Right(module -> Repository(repoName, previousRepo))
      } else {
        Left(s"cannot add module $module because $lastModule already exists in $repoName for $previousRepo")
      }
    }
    
    override def hashWasAdded(lastModule: Module, previousRepo: Int)(implicit session: Session): Either[String, (Module, Repository)] = {
      if (module == lastModule) {
        Right(module -> Repository(repoName, previousRepo))
      } else {
        Left(s"cannot add module $module because $lastModule already exists in $repoName for $previousRepo")
      }
    }
    
    override def noHashFound(implicit session: Session): Either[String, (Module, Repository)] = {
      currentVersion(repoName).map{ currentRepo =>
        Modules.insert(Modules.toRow(module, repoName, currentRepo, Changes.Insert))
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
    def hashWasUpdated(lastModule: Module, previousRepo: Int)(implicit session: Session): Either[String, (Module, Repository)]
    
    /** this module was added in a previous version */ 
    def hashWasAdded(lastModule: Module, previousRepo: Int)(implicit session: Session): Either[String, (Module, Repository)]

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
      val thisModuleQ = Query(Modules).filter(m => m.hash === hash.value && m.repoName === repoName).sortBy(_.repoVersion.desc)
      val maybeFoundModuleRepo = thisModuleQ.take(1).firstOption.map(Modules.fromRow)
      val maybeFoundVersion = maybeFoundModuleRepo.map{ case (n, r) => r.version}
      println(maybeFoundModuleRepo)
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
          val latestVersion = onlyOption(Query(Query(RepositoryVersions).filter(_.name === repoName).map(_.version).max)).get
          val lastModuleQ = Query(Modules)
            .filter(m => 
              m.hash === hash.value && m.repoName === repoName && m.repoVersion === latestVersion)
          val ((lastModule, lastRepo), lastChange) = onlyOption(lastModuleQ).map(t => Modules.fromRow(t) -> t._11).get
          if (lastChange == Changes.Deleted) {
            execute.hashWasRemoved(lastModule, lastRepo.version)
          } else if (lastChange == Changes.Updated) {
            execute.hashWasUpdated(lastModule, lastRepo.version)
          } else if (lastChange == Changes.Insert) {
            execute.hashWasAdded(lastModule, lastRepo.version)
          } else throw new Exception(s"FATAL: could not find the type for the latest change: $lastChange in repo: $lastRepo")
        }
      }.getOrElse{
        execute.noHashFound
      }
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
  
  
  private def activeModulesForRepo(repo: Repository)(implicit session: Session): List[Module] = {
    val modules = for {
      m <- Modules
      if m.repoName === repo.name &&
        (m.repoVersion === repo.version && 
        (m.change.isNull || m.change === Changes.Updated))
    } yield m
    modules.list.map( t => Modules.fromRow(t) match { 
      case (module, repo) => module 
    })
  }
  
  private def verifyArtifacts(module: Module, doCheck: Boolean): Either[String, Unit] = {
    Right() //TODO
  }
  
  private def verifyDeps(module: Module, repoName: String, activeVersion: Int)(implicit session: Session): Either[String, Unit] = {
    val deletedHashes = Query(Modules).filter{ m => 
      m.hash.inSet(module.deps.map(_.value)) && 
      (m.change === Changes.Deleted) && 
      m.repoName === repoName &&
      (m.repoVersion === Query(m.repoVersion).max && m.repoVersion <= activeVersion)
    }.map(m => m.hash).list
    
    val missingHashes = module.deps.filter{ depHash =>
      val exists = Query(Query(Modules)
          .filter(m => 
            m.hash === depHash.value && 
            m.repoName === repoName &&
            (m.change === Changes.Updated || m.change.isNull))
          .exists).firstOption.getOrElse(false)
      !exists
    } ++ deletedHashes
    if (missingHashes.size > 0) {
      Left(s"missing dependencies in test for versions up to 0. These hashes were not found: ${missingHashes.mkString(",")}")
    } else Right()
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
          println(activeModules.mkString("\n"))
          val modulesCheck = activeModules.foldLeft(Right(): Either[String, Unit])( (current, thisModule) => 
            for {
              c <- current.right
              //TODO:_ <- verifyArtifacts(thisModule, checkArtifacts).right
              _ <- verifyDeps(thisModule, name, activeVersion).right
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
