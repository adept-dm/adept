package adept.api

import java.io.{File => jFile}
import slick.session.Database

//database
import Database.threadLocalSession
import db.driver.simple._

case object RemoveThisNotImplementedException extends Exception


object Adept {
  def list(name: String)(implicit database: Database): Either[String, String]= {
    database.withSession{
      val q = for {
          (d,m) <- Modules leftJoin Metadata on (_.hash === _.moduleHash)
        } yield (d.hash, d.org, d.name, d.version, m.key.?, m.value.?)
      
      Right(MetaModuleRow.formatModules(MetaModuleRow.fromList(q.list)).mkString("\n"))
    }
  }

  def init(name: String)(implicit database: Database): Either[String, String]= {
    database.withSession{
      val currentRow = {
        val s = implicitly[Session]
        val tables = s.conn.getMetaData().getTables(s.conn.getCatalog(), null, null, null)
        tables.last()
        tables.getRow()
      }
      if (currentRow > 28) { //28 is the number of tables in h2 on init
        val repository = Query(RepositoryMetadata).filter(_.name === name).firstOption
        if (repository.isDefined) {
          Left(s"repository $name is already defined")
        } else {
          RepositoryMetadata.autoInc.insert(name)
          Right(s"Initialized adept repository $name ")
        }
      } else {
        db.allDDLs.create
        RepositoryMetadata.autoInc.insert(name)
        Right(s"Created new adept repository $name")
      }
    }
  }
  
  def add(repoName: String, module: Module, dependencies: Seq[Module])(implicit database: Database): Either[String, Module] = {
    
    def metadataInsert(module: Module) = Metadata.autoInc.insertAll{
      module.metadata.data.map{ case (key, value) =>
        (key, value, module.hash.value)
      }.toSeq : _*
    }
    
    database.withTransaction{
      val repoQuery = Query(Query(RepositoryMetadata).filter(_.name === repoName).map(_.version).max)
      (repoQuery.firstOption.flatten.map { repoId =>
        val moduleExists = (for {
          d <- Modules
          if d.hash === module.hash.value
        } yield d.hash).firstOption.isDefined
        val newDependencyHashes = dependencies.map(_.hash.value)

        if (moduleExists) {
          val metadata = (for {
            m <- Metadata
            if m.moduleHash === module.hash.value
          } yield m.key -> m.value).list
          
          val dependencyHashes = (for {
            module <- Modules
            dep <- Dependencies
            if dep.parentHash === module.hash
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
            Modules.autoInc.insert(Modules.toRow(module, repoId))
            metadataInsert(module)
            Dependencies.autoInc.insertAll(dependencies.map(_.hash).map(childHash => module.hash.value -> childHash.value): _*)
            Right(module)
          } else {
            Left(s"cannot insert ${module} with deps: {${newDependencyHashes.mkString(",")}} because these deps are not known to adept: {${newDependencyHashes.diff(existingHashes).mkString(",")}}")
          }
          
        }
      }).toRight(s"could not find a repository named: $repoName").joinRight
    }
  }

  //TODO: move this class and companion object out of here, not very happy with naming: consider changing it
  case class MetaModuleRow(hash: String, org: String, name: String, version: String, metaKey: Option[String], metaValue: Option[String]) 
  
  object MetaModuleRow {
    def fromList(l: List[(String, String, String, String, Option[String],Option[String])]) = {
      l.map(r => MetaModuleRow(r._1, r._2, r._3, r._4, r._5, r._6))
    }
    
    def formatModules(values: List[MetaModuleRow]): Seq[Module] = {
      values.groupBy(r => (r.hash, r.org, r.name, r.version)).toSeq.map{
        //for each hash we have:
        case ((hash, org, name, version), rest) => {
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
          Module(coords, allMetadata, Hash(hash))
        }
      }
    }
  }
  
  def describe(coords: Coordinates, meta: Metadata)(implicit database: Database): Either[String, (Module, Seq[Module])] = {
    database.withSession{
      val q = for {
          (d,m) <- Modules leftJoin Metadata on (_.hash === _.moduleHash)
          if d.name === coords.name && 
             d.org === coords.org &&
             d.version === coords.version
        } yield (d.hash, d.org, d.name, d.version, m.key.?, m.value.?)
     MetaModuleRow.formatModules(MetaModuleRow.fromList(q.list))
      .filter{ //TODO: filter in query?
        d => meta.data.isEmpty || d.metadata == meta
      }.headOption.map{ module =>
        module -> {
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
          MetaModuleRow.formatModules(MetaModuleRow.fromList(q.list)).filter( _ != module)
        }
      }.toRight(s"could not describe $coords$meta")
    }
  }
  
  def pull(repos: Seq[String], dir: jFile) = {
    throw RemoveThisNotImplementedException
  }

  def push(repos: Seq[String]) = {
    throw RemoveThisNotImplementedException
  }


  def download(coords: Coordinates, hash: Hash, overwrite: Boolean = false) = {
    throw RemoveThisNotImplementedException
  }
}
