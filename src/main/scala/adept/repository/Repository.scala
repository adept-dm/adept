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
  def init(dir: jFile)= {
    database.withSession{ //TODO: check if schema is there and schema version 
      (Metadata.ddl ++ Descriptors.ddl ++ RepositoryMetadata.ddl ++ Dependencies.ddl).create 
    }
  }
  
  def add(descriptor: Descriptor, file: jFile): Either[String, Descriptor] = {
    val repoId = 1 //FIXME: only increment repoId when pushed/merged RepositoryMetadata.autoInc.insert(new java.sql.Timestamp((new java.util.Date).getTime()))
    
    def metadataInsert(descriptor: Descriptor) = Metadata.autoInc.insertAll{
      descriptor.metadata.data.map{ case (key, value) =>
        (key, value, descriptor.hash.value)
      }.toSeq : _*
    }
    
    database.withTransaction{
      val descriptorExists = (for {
        d <- Descriptors
        if d.hash === descriptor.hash.value
      } yield d.hash).firstOption.isDefined
      
      if (descriptorExists) {
        val metadata = (for {
          m <- Metadata
          if m.descriptorHash === descriptor.hash.value
        } yield m.key -> m.value).list
        if (metadata.toMap == descriptor.metadata.data) {
          Right(descriptor)
        } else {
          Left(s"cannot insert ${descriptor.metadata} because ${descriptor.coords} has ${Metadata(metadata.toMap)} already" )
        }
      } else {
        Descriptors.autoInc.insert(Descriptors.toRow(descriptor, repoId))
        metadataInsert(descriptor)
        Right(descriptor)
      }
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
        else lastHash.foreach{ lh => assert(lh == hash, s"FATAL ERROR: found multiple hashes ($lh and $hash) bound to the same set of coordinates $coords") }

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
      }.headOption.toRight(s"could not descrive $coords$meta")
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