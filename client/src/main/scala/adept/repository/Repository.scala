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

  def describe(coords: Coordinates, meta: Metadata): Either[String, Seq[Descriptor]] = {
    
    Left("TODO")
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