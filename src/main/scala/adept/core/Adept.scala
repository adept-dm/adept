package adept.core

import java.io.{File => jFile}
import slick.session.Database

//database
import Database.threadLocalSession
import db.driver.simple._

object Adept {
  def list(name: String)(implicit database: Database): Either[String, Seq[(Module, Repository)]]= {
    database.withSession{
      val q = for {
          (d,m) <- Modules leftJoin Metadata on (_.hash === _.moduleHash)
          rm <- RepositoriesModules
          if d.hash === rm.hash
        } yield (d.hash, d.org, d.name, d.version, m.key.?, m.value.?, rm.name, rm.version)
      Right(
          MetaModuleRow
            .formatModules(MetaModuleRow.fromListWithRepo(q.list))
            .map{case (module, repoOpt) => module -> repoOpt.get} //should be safe
            .sortBy(r => r._2.toString)
      )
    }
  }
  
  def repoList(implicit database: Database): Seq[(Repository, Option[Int])]= {
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
    }
  }

  def init(name: String)(implicit database: Database): Either[String, String]= {
    database.withTransaction{
      if (db.checkExistence(implicitly[Session])) {
        val repository = Query(RepositoryMetadata).filter(_.name === name).firstOption
        if (repository.isDefined) {
          Left(s"repository $name is already defined")
        } else {
          RepositoryMetadata.insert(name, 0)
          Right(s"Initialized adept repository $name ")
        }
      } else {
        db.allDDLs.create
        RepositoryMetadata.insert(name, 0)
        Right(s"Created new adept repository $name")
      }
    }
  }
  
  /** currently diff output all staged modules 
    *  
    * TODO: add rm, and diffing between repository versions  
    */
  def diff(repoName: String)(implicit database: Database): Either[String, Seq[Module]] = {
    database.withSession{
      val currentRepo = (for {
        repo <- StagedRepositories
        if repo.name === repoName
      } yield repo).firstOption
      currentRepo.map{ case (repoName, repoVersion)=> 
        
        val stagedModules = for {
          rm <- RepositoriesModules
          (d,m) <- Modules leftJoin Metadata on (_.hash === _.moduleHash)
          if rm.name === repoName && rm.version === repoVersion && rm.hash === d.hash 
        } yield (d.hash, d.org, d.name, d.version, m.key.?, m.value.?)
        
        MetaModuleRow.formatModules(MetaModuleRow.fromList(stagedModules.list)).map {
          case (module, repo) => module
        }
        
      }.toRight(s"nothing staged in $repoName")
    }
  }
  
  //TODO: move away from here
  private def latestRepoQuery(repoName: String)(implicit s: Session) = Query(Query(RepositoryMetadata).filter(_.name === repoName).map(_.version).max) 
  
  /** creates a new repository version from the currently staged repository
    *  
    * in practice this means to move all modules attached to the staged repository
    * to a real repository
    */
  def commit(repoName: String)(implicit database: Database): Either[String, Repository] = {
    database.withTransaction{
      val currentStagedQ = Query(StagedRepositories).filter(_.name === repoName)
      val current = currentStagedQ.firstOption
      current.map{ case (name, version) =>
        RepositoryMetadata.insert(name, version)
        currentStagedQ.delete
        Repository(name, version)
      }.toRight("nothing to commit")
    }
  }
  
  /** adds a module to a repository  */
  def add(repoName: String, module: Module, dependencies: Seq[Module])(implicit database: Database): Either[String, Module] = {
    
    def metadataInsert(module: Module) = Metadata.insertAll{
      module.metadata.data.map{ case (key, value) =>
        (key, value, module.hash.value)
      }.toSeq : _*
    }
    
    database.withTransaction{
      
      //find the current staged repository or create a new one
      def stagedRepository = { 
        val currentStage = Query(StagedRepositories).filter(_.name === repoName).firstOption //there is only one staged repo per name
        if (currentStage.isEmpty) { //first time we add on this commit
          val latestRepo = latestRepoQuery(repoName).firstOption.flatten
          def updatedVersion(v: Int) = v + 1
          latestRepo.foreach{ v => StagedRepositories.insert(repoName, updatedVersion(v) ) }
          latestRepo.map( v => repoName -> updatedVersion(v))
        } else { //we have a staged repository
          currentStage
        }
      }
      
      (stagedRepository.map { case (repoName, repoVersion) =>
        val moduleExists = (for {
          rm <- RepositoriesModules
          if rm.name === repoName && rm.hash === module.hash.value
          dbModule <- Modules
          if dbModule.hash === module.hash.value
        } yield dbModule.hash).firstOption.isDefined
        val newDependencyHashes = dependencies.map(_.hash.value)

        if (moduleExists) {
          val metadata = (for {
            m <- Metadata
            if m.moduleHash === module.hash.value
          } yield m.key -> m.value).list
          
          val dependencyHashes = (for {
            dep <- Dependencies
            if dep.parentHash === module.hash.value
          } yield {
            dep.childHash
          }).list
          
          if (metadata.toMap == module.metadata.data && dependencyHashes == newDependencyHashes) {
            Right(module)
          } else {
            Left(s"cannot insert ${module} with deps: {${newDependencyHashes.mkString(",")}} because ${module.hash} already has meta: ${Metadata(metadata.toMap)} and deps: {${dependencyHashes.mkString(",")}}" )
          }
        } else {
          val existingHashes = (for {
            module <- Modules
            if module.hash.inSet(newDependencyHashes)
          } yield {
            module.hash
          }).list
          if (existingHashes.sorted == newDependencyHashes.sorted) {
            val existsInRepo = (for {
              rm <- RepositoriesModules
              if rm.name === repoName && rm.hash === module.hash.value
            } yield {
              rm.hash
            }).firstOption.isDefined
            println("inserting into: "+ repoName + " " + existsInRepo)
            
            if (existsInRepo) {
              Modules.insert(Modules.toRow(module))
              metadataInsert(module)
              Dependencies.insertAll(dependencies.map(_.hash).map(childHash => module.hash.value -> childHash.value): _*)
            }
            RepositoriesModules.insert(repoName, repoVersion, module.hash.value)
            Right(module)
          } else {
            Left(s"cannot insert ${module} with deps: {${newDependencyHashes.mkString(",")}} because these deps are not known to adept: {${newDependencyHashes.diff(existingHashes).mkString(",")}}")
          }
          
        }
      }).toRight(s"could not find a repository named: $repoName").joinRight
    }
  }

  //TODO: move this class and companion object out of here, not very happy with naming: consider changing it
  case class MetaModuleRow(hash: String, org: String, name: String, version: String, metaKey: Option[String], metaValue: Option[String], repo: Option[Repository] = None) 
  
  object MetaModuleRow {
    def fromList(l: List[(String, String, String, String, Option[String],Option[String])]) = {
      l.map(r => MetaModuleRow(r._1, r._2, r._3, r._4, r._5, r._6))
    }

    def fromListWithRepo(l: List[(String, String, String, String, Option[String],Option[String], String, Int)]) = {
      l.map(r => MetaModuleRow(r._1, r._2, r._3, r._4, r._5, r._6, Some(Repository(r._7, r._8))))
    }
    
    def formatModules(values: List[MetaModuleRow]): Seq[(Module, Option[Repository])] = {
      values.groupBy(r => (r.hash, r.org, r.name, r.version, r.repo)).toSeq.map{
        //for each hash we have:
        case ((hash, org, name, version, repo), rest) => {
          val coords = Coordinates(org, name, version)
          //gather the meta key/value pairs for this hash:
          val allMetadata = Metadata((rest.groupBy(_.metaKey).flatMap{
            case (_, allProps) => {
              allProps.flatMap{ p => for {
                key <- p.metaKey
                value <- p.metaValue
              } yield (key -> value)}
            }
          }).toMap)
          Module(coords, allMetadata, Hash(hash)) -> repo
        }
      }
    }
  }
  
  /** returns the module and all its dependencies matching the coordinates and metadata */
  def describe(coords: Coordinates, meta: Metadata)(implicit database: Database): Either[String, (Module, Seq[Module])] = {
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
    }
  }
  
}
