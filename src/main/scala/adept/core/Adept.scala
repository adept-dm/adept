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
  
  /** currently diff output all staged modules 
    *  
    * TODO: add rm, and diffing between repository versions  
    */
  def diff(repoName: String, from: Option[Int] = None, to: Option[Int] = None)(implicit database: Database): Either[String, Seq[Module]] = {
    /*
    database.withSession{
      val currentRepo = (for {
        repo <- StagedRepositories
        if repo.name === repoName
      } yield repo).firstOption
      currentRepo.map{ case (repoName, repoVersion)=> 
        val stagedModules = for {
          (d,m) <- Modules leftJoin Metadata on (_.hash === _.moduleHash)
          if d.repoName === repoName && d.repoVersion === repoVersion
        } yield (d.hash, d.org, d.name, d.version, m.key.?, m.value.?)
        
        MetaModuleRow.formatModules(MetaModuleRow.fromList(stagedModules.list)).map {
          case (module, repo) => module
        }
        
      }.toRight(s"nothing staged in $repoName")
    }*/
    null
  }
  
  private def currentVersion(repoName: String)(implicit session: Session): Option[Int] = { 
    val currentRepo = Query(RepositoryVersions)
        .filter(r => r.name === repoName)
    val active = currentRepo.filter(_.active).list
    if (active.length == 1) {
      currentRepo.map(_.version).firstOption
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
  
  /** removes the module with hash from a repository  */
  def remove(repoName: String, hash: Hash)(implicit database: Database) = change(repoName, hash, None)
  
  /** updates the module with hash in a repository  */
  def update(repoName: String, hash: Hash, newModule: Module)(implicit database: Database) = change(repoName, hash, Some(newModule))
  
  private def change(repoName: String, hash: Hash, maybeNewModule : Option[Module])
    (implicit database: Database): Either[String, Module] = {
    import Database.threadLocalSession
    
    database.withTransaction{
      notDirty(repoName){
        val moduleQ: Query[Modules.type, Modules.ModuleType] = latestModule(hash, repoName)
        val module = moduleQ
          .take(1)
          .firstOption.map( Modules.fromRow )
          
        module.map{ case (module, repo) =>
          val activeRepo = Query(RepositoryVersions)
            .filter(r => r.name === repo.name && r.version === repo.version && r.active === true)
            .take(1).firstOption

          val isUpdate = maybeNewModule .isDefined
          if (activeRepo.isDefined) { 
            if (isUpdate) {
              val Some(newModule) = maybeNewModule 
              moduleQ.update(Modules.toRow(newModule, repo.name, repo.version, Changes.Insert))
              Right(newModule)
            } else { //isDelete
              moduleQ.delete
              Right(module)
            }
          } else {
            currentVersion(repoName).map{ repoVersion =>
              if (isUpdate) {
                val Some(newModule) = maybeNewModule 
                Modules.insert(Modules.toRow(newModule, repoName, repoVersion, Changes.Updated))
                Right(newModule)
              } else { //isDelete
                Modules.insert(Modules.toRow(module, repoName, repoVersion, Changes.Deleted))
                Right(module)
              }
            }.toRight(s"could not find current versin of $repoName for $module in a non-staged repository").joinRight
          }
        }.toRight(s"could not find $hash").joinRight
      }.joinRight
    }
  }

  private def latestModule(hash: Hash, repoName: String) = {
    for {
      m <- Modules 
      if m.version === Query(m.version).max &&
         m.hash === hash.value &&
         m.repoName === repoName
    } yield m
  }
  
  //chechs if this is a repository where there is work
  private def notDirty[A](repoName: String)(block: => A): Either[String, A] = {
    import Database.threadLocalSession
    val dirty = Query(Query(RepositoryVersions).filter(r => r.name === repoName && r.stashed).exists)
      .firstOption.getOrElse(true)
    if (dirty) Left(s"cannot change repository $repoName, because it contains stashed versions and is considred dirty")
    else Right(block)
  }
  
  /** adds a module to a repository  */
  def add(repoName: String, module: Module)(implicit database: Database): Either[String, Module] = {
    import Database.threadLocalSession
    
    database.withTransaction{
      notDirty(repoName){
        val inserted = latestModule(module.hash, repoName)
            .filter(_.change.isNull)
            .firstOption.map( Modules.fromRow )
        val thisModule = (if (inserted.isEmpty) {
          currentVersion(repoName).map{ repoVersion =>
            Modules.insert(Modules.toRow(module, repoName, repoVersion, Changes.Insert))
            Right(module)
          }.toRight(s"could not find any versions in repository $repoName. not initialized?").joinRight
        } else {
          val (existingModule, oldRepo) = inserted.get
          if (module == existingModule) {
            Right(module)
          } else {
            Left(s"could not insert $module because $existingModule exists with the same hash exists in version ${oldRepo.version}")
          }
        })
        thisModule
      }.joinRight
    }
  }
  
  private def modulesForRepo(repo: Repository)(implicit session: Session): List[Module] = {
    val modules = for {
      m <- Modules
      if m.repoName === repo.name &&
      (m.repoVersion === Query(m.repoVersion).max && m.repoVersion <= repo.version && 
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
      (m.change === false) && 
      m.repoName === repoName &&
      (m.repoVersion === Query(m.repoVersion).max && m.repoVersion <= activeVersion)
    }.map(m => m.hash).list
    
    val missingHashes = module.deps.filter{ depHash =>
      val exists = Query(Query(Modules)
          .filter(m => 
            m.hash === depHash.value && 
            m.repoName === repoName &&
            (m.change === true || m.change.isNull))
          .exists).firstOption.getOrElse(false)
      !exists
    } ++ deletedHashes
    if (missingHashes.size > 0) {
      Left(s"missing dependencies in test for versions up to 0. These hashes were not found: ${missingHashes.mkString(",")}")
    } else Right()
  }
  
  
   /** creates a new repository version from the currently staged repository
    */
  def commit(repoName: String, checkArtifacts: Boolean = true)(implicit database: Database): Either[String, Repository] = {
    import Database.threadLocalSession

    database.withTransaction{
      notDirty(repoName){
        val activeQ = Query(RepositoryVersions)
          .filter(r => r.name === repoName).filter(_.active)
        val active = activeQ.list
        if (active.length == 1) {
          val (name, activeVersion, _, _) = active.headOption.get
          val modules = modulesForRepo(Repository(name, activeVersion))
          val modulesCheck = modules.foldLeft(Right(): Either[String, Unit])( (current, thisModule) => 
            for {
              c <- current.right
              _ <- verifyArtifacts(thisModule, checkArtifacts).right
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
    /*
    database.withSession{
      val q = for {
          (dbModule,m) <- Modules leftJoin Metadata on (_.hash === _.moduleHash)
          if dbModule.name === coords.name && 
             dbModule.org === coords.org &&
             dbModule.version === coords.version
        } yield (dbModule.hash, dbModule.org, dbModule.name, dbModule.version, m.key.?, m.value.?)
     MetaModuleRow.formatModules(MetaModuleRow.fromList(q.list))
      .filter{ //TODO: filter in query?
          case (d, _) => meta.data.isEmpty || d.metadata == meta
      }.headOption.map{ case (module, r) =>
        module -> {
          
          //find all dependencies from the hashes
          @tailrec def allDeps(hashes: Seq[String]): Seq[String] = {
            //TODO: perhaps do once on the DB instead?
            val childHashes = Query(Dependencies).filter(_.parentHash.inSet(hashes)).map(_.childHash).list
            val newChildren = childHashes.diff(hashes)
            if (newChildren.isEmpty)
               hashes
            else {
               allDeps(newChildren) ++ hashes
            } 
          }
          val dependencyHashes = allDeps(Seq(module.hash.value))
          val q = for {
            (d,m) <- Modules leftJoin Metadata on (_.hash === _.moduleHash)
            if d.hash inSet(dependencyHashes)
          } yield (d.hash, d.org, d.name, d.version, m.key.?, m.value.?)
          MetaModuleRow.formatModules(MetaModuleRow.fromList(q.list)).map{ case (module, repo) => module }.filter( _ != module)
        }
      }.toRight(s"could not describe $coords$meta")
    }*/
    null
  }
  
}
