package adept.repository

import java.io.{File => jFile}
import adept.client.Configuration
import slick.session.Database

//database
import Database.threadLocalSession
import db.driver.simple._
import db._

case object RemoveThisNotImplementedException extends Exception


object Repository {
  def init(dir: jFile, name: String): Either[String, String]= {
    database.withSession{ //TODO: check if schema is there and schema version
      val currentRow = {
        val s = implicitly[Session]
        val tables = s.conn.getMetaData().getTables(s.conn.getCatalog(), null, null, null)
        tables.last()
        tables.getRow()
      }
      if (currentRow > 28) { //28 is the number of tables in h2 on init
        val repository = Query(RepositoryMetadata).filter(_.name === name).firstOption
        if (repository.isDefined) {
          Left(s"repository $name is already defined in $dir")
        } else {
          RepositoryMetadata.autoInc.insert(name)
          Right(s"Initialized adept repository: $name in $dir")
        }
      } else {
        (Metadata.ddl ++ Descriptors.ddl ++ RepositoryMetadata.ddl ++ Dependencies.ddl).create
        RepositoryMetadata.autoInc.insert(name)
        Right(s"Created new adept repository: $name in $dir")
      }
    }
  }
  
  def add(repoName: String, descriptor: Descriptor, file: jFile, dependencies: Seq[Descriptor]): Either[String, Descriptor] = {
    
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

  def describe(coords: Coordinates, meta: Metadata): Either[String, Descriptor] = {
    database.withSession{
      val q = for {
          (d,m) <- Descriptors leftJoin Metadata on (_.hash === _.descriptorHash)
          if d.name === coords.name && 
             d.org === coords.org &&
             d.version === coords.version
        } yield (d.hash, d.name, d.org, d.version, m.key.?, m.value.?)
      val values = q.list
      var lastHash: Option[String] = None //TODO: check this is in query?
      values.map{ case (hash, _, _, _, _, _) =>
        if (lastHash == None) lastHash = Some(hash)
        else lastHash.foreach{ lh => assert(lh == hash, s"FATAL ERROR: found multiple hashes ($lh and $hash) bound to the same set of coordinates $coords with meta $meta. list of values for debug: $values") }

        val allMetadata = Metadata((values.groupBy(_._5).flatMap{ //_5 is key
          case (_, allProps) => {
            allProps.flatMap{ p => for {
              key <- p._5
              value <- p._6
            } yield (key -> value)}
          }
        }).toMap)
        Descriptor(coords, allMetadata, Hash(hash))
      }.filter{ //TODO: filter in query?
        _.metadata == meta
      }.headOption.toRight(s"could not describe $coords$meta")
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