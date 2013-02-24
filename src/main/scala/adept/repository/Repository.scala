package adept.repository

import java.io.{File => jFile}
import adept.client.Configuration
import slick.session.Database

//database
import Database.threadLocalSession
import db.driver.simple._

case object RemoveThisNotImplementedException extends Exception


object Repository {
  def list(name: String)(database: Database): Either[String, String]= {
    database.withSession{
      val q = for {
          (d,m) <- Descriptors leftJoin Metadata on (_.hash === _.descriptorHash)
        } yield (d.hash, d.org, d.name, d.version, m.key.?, m.value.?)
      
      Right(MetaDescriptorRow.formatDescriptors(MetaDescriptorRow.fromList(q.list)).mkString("\n"))
    }
  }
  
  def init(name: String)(database: Database): Either[String, String]= {
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
        (Metadata.ddl ++ Descriptors.ddl ++ RepositoryMetadata.ddl ++ Dependencies.ddl).create
        RepositoryMetadata.autoInc.insert(name)
        Right(s"Created new adept repository $name")
      }
    }
  }
  
  def add(repoName: String, descriptor: Descriptor, dependencies: Seq[Descriptor])(database: Database): Either[String, Descriptor] = {
    
    def metadataInsert(descriptor: Descriptor) = Metadata.autoInc.insertAll{
      descriptor.metadata.data.map{ case (key, value) =>
        (key, value, descriptor.hash.value)
      }.toSeq : _*
    }
    
    database.withTransaction{
      val repoQuery = Query(Query(RepositoryMetadata).filter(_.name === repoName).map(_.version).max)
      (repoQuery.firstOption.flatten.map { repoId =>
        val descriptorExists = (for {
          d <- Descriptors
          if d.hash === descriptor.hash.value
        } yield d.hash).firstOption.isDefined
        
        if (descriptorExists) {
          val metadata = (for {
            m <- Metadata
            if m.descriptorHash === descriptor.hash.value
          } yield m.key -> m.value).list
          
          val dependencyHashes = (for {
            descriptor <- Descriptors
            dep <- Dependencies
            if dep.parentHash === descriptor.hash
          } yield {
            dep.childHash
          }).list
          if (metadata.toMap == descriptor.metadata.data && dependencyHashes == dependencies.map(_.hash.value)) {
            Right(descriptor)
          } else {
            Left(s"cannot insert ${descriptor.metadata} because ${descriptor.coords} it already exsits with: ${Metadata(metadata.toMap)} and the following dependencies: ${dependencyHashes.mkString(",")}" )
          }
        } else {
          Descriptors.autoInc.insert(Descriptors.toRow(descriptor, repoId))
          metadataInsert(descriptor)
          Dependencies.autoInc.insertAll(dependencies.map(_.hash).map(childHash => descriptor.hash.value -> childHash.value): _*)
          Right(descriptor)
        }
      }).toRight(s"could not find a repository named: $repoName").joinRight
    }
  }

  //TODO: move this class and companion object out of here, not very happy with naming: consider changing it
  case class MetaDescriptorRow(hash: String, org: String, name: String, version: String, metaKey: Option[String], metaValue: Option[String]) 
  
  object MetaDescriptorRow {
    def fromList(l: List[(String, String, String, String, Option[String],Option[String])]) = {
      l.map(r => MetaDescriptorRow(r._1, r._2, r._3, r._4, r._5, r._6))
    }
    
    def formatDescriptors(values: List[MetaDescriptorRow]): Seq[Descriptor] = {
      values.groupBy(r => (r.hash, r.org, r.name, r.version)).toSeq.map{
        case ((hash, org, name, version), rest) => {
          val coords = Coordinates(org, name, version)
          val allMetadata = Metadata((rest.groupBy(_.metaKey).flatMap{
            case (_, allProps) => {
              allProps.flatMap{ p => for {
                key <- p.metaKey
                value <- p.metaValue
              } yield (key -> value)}
            }
          }).toMap)
          Descriptor(coords, allMetadata, Hash(hash))
        }
      }
    }
  }
  
  def describe(coords: Coordinates, meta: Metadata)(database: Database): Either[String, (Descriptor, Seq[Descriptor])] = {
    database.withSession{
      val q = for {
          (d,m) <- Descriptors leftJoin Metadata on (_.hash === _.descriptorHash)
          if d.name === coords.name && 
             d.org === coords.org &&
             d.version === coords.version
        } yield (d.hash, d.org, d.name, d.version, m.key.?, m.value.?)
     MetaDescriptorRow.formatDescriptors(MetaDescriptorRow.fromList(q.list))
      .filter{ //TODO: filter in query?
        d =>
          meta.data.isEmpty || d.metadata == meta
      }.headOption.map{ descriptor =>
        descriptor -> {
          val dependencyHashes = Query(Dependencies).filter(_.parentHash === descriptor.hash.value).map(_.childHash).list
          val q = for {
            (d,m) <- Descriptors leftJoin Metadata on (_.hash === _.descriptorHash)
            if d.hash inSet(dependencyHashes)
          } yield (d.hash, d.org, d.name, d.version, m.key.?, m.value.?)
          MetaDescriptorRow.formatDescriptors(MetaDescriptorRow.fromList(q.list))
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

  def location(coords: Coordinates, hash: Hash, noCache: Boolean = false): Seq[Location] = {
    throw RemoveThisNotImplementedException
  }


  def download(coords: Coordinates, hash: Hash, overwrite: Boolean = false) = {
    throw RemoveThisNotImplementedException
  }
}