package adept.core

import slick.lifted.ForeignKeyAction
import slick.session._
import java.sql.Timestamp

object db {
  lazy val database = {
    Database.forURL("jdbc:h2:"+Configuration.currentAdeptDir()+"/adept", driver = "org.h2.Driver") 
  }

  lazy val driver = slick.driver.H2Driver
  
  lazy val allDDLs = {
    import driver._
    Modules.ddl ++ RepositoryVersions.ddl
  }
  
  //true if DB exists here
  def checkExistence(s: Session) = {
    val currentRow = {
      val tables = s.conn.getMetaData().getTables(s.conn.getCatalog(), null, null, null)
      tables.last()
      tables.getRow()
    }
    (currentRow > 28) //h2 has 28 rows on init
  }
}

import db.driver.simple._

package object types {  
  type ModulesType = (String, String, String, String, String, String, String, String, String, Int, Boolean)
  type RepositoryVersionsType = (String, Int, Option[String], Boolean, Boolean)
}
import types._

object Modules extends Table[ModulesType]("MODULES") {
  def hash = column[String]("HASH", O.NotNull)
  def org = column[String]("ORG", O.NotNull)
  def name = column[String]("NAME", O.NotNull)
  def version = column[String]("VERSION", O.NotNull)
  def artifactHash = column[String]("ARTIFACT_HASH", O.NotNull)

  //TODO: I am not sure about representing the ones below as strings instead of tables- it does save space and make it easier wrt versioning:
  def metadata = column[String]("METADATA")
  def childHashes = column[String]("CHILD_HASHES")
  def artifacts = column[String]("ARTIFACTS", O.NotNull)

  def deleted = column[Boolean]("MODULES_DELETED")
  def repoName = column[String]("MODULES_REPO_NAME", O.NotNull)
  def repoVersion = column[Int]("MODULES_REPO_VERSION", O.NotNull)
  
  //TODO: fix the order of params so that it matches Module
  def * = hash ~ org ~ name ~ version ~ artifactHash ~ artifacts ~ metadata ~ childHashes ~ repoName ~ repoVersion ~ deleted
   
  //TODO: add childHashes
  def hashIdx= index("MODULE_HASH_INDEX", (hash, repoName, repoVersion), unique = true)
  
  def toRow(module: Module, repoName: String, repoVersion: Int, deleted: Boolean)
  :  ModulesType = {
    (module.hash.value, module.coords.org, module.coords.name, module.coords.version, module.artifactHash.value,
        Parsers.setToString(module.artifacts), module.metadata.toString, Parsers.setToString(module.deps), 
        repoName, repoVersion, deleted)
  }
    
        
  def fromRow(t: ModulesType) = {
    val (hashString, org, name, version, artifactHashString, artifactsString, metadataString, depsString, repoName, repoVersion, deleted) = t
    
    val hash = Hash(hashString)
    val artifactHash = Hash(artifactHashString)
    val coords = Coordinates(org, name, version)
    val artifacts = Parsers.setFromString(artifactsString).fold(
        error => throw new Exception("FATAL: while parsing DB got: " + error),
        strings => strings
    ).map( Artifact.apply )
    val metadata = Parsers.metadata(metadataString).left.map{
      _ => throw new Exception(s"FATAL: could not parse metadata in DB for: $hash. Got: $metadataString")
    }.right.get
    val deps = Parsers.setFromString(depsString).fold(
        error => throw new Exception("FATAL: while parsing DB got: " + error),
        strings => strings
    ).map( Hash.apply )
    
     //TODO: add delete here:
    (Module(coords, metadata, hash, artifactHash, artifacts,  deps), VersionId(repoName, repoVersion))
  }
}

object RepositoryVersions extends Table[RepositoryVersionsType]("REPOSITORY_VERSIONS") {
  def name = column[String]("NAME", O.NotNull)
  def version = column[Int]("VERSION", O.NotNull)
  def hash = column[Option[String]]("REPO_HASH")
  def active = column[Boolean]("ACTIVE", O.NotNull)
  def stashed = column[Boolean]("STASHED", O.NotNull)
  def * = name ~ version ~ hash ~ active ~ stashed
  
  def nameVersionIdx= index("R_NAME_VERSION_UNIQUE_INDEX", (name, version), unique = true)
  
  def fromRow(t: RepositoryVersionsType) = {
    (Repository(VersionId(t._1, t._2), t._3.map(Hash.apply)), t._4, t._5)
  }
}
