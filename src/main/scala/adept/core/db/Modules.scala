package adept.core.db

import adept.core.models._
import adept.core.db.DAO.driver.simple._
import adept.core.db.Types._
import adept.core.parsers._

import slick.lifted.ForeignKeyAction
import slick.session._
import java.sql.Timestamp


object Modules extends Table[ModulesType]("MODULES") { //TODO: rename to changes
  def hash = column[String]("HASH", O.NotNull)
  def org = column[String]("ORG", O.NotNull)
  def name = column[String]("NAME", O.NotNull)
  def version = column[String]("VERSION", O.NotNull)
  def artifactHash = column[String]("ARTIFACT_HASH", O.NotNull)

  //TODO: I am not sure about representing the ones below as strings instead of tables- it does save space and make it easier wrt versioning:
  def metadata = column[String]("METADATA")
  def depHashes = column[String]("DEPS")
  def artifacts = column[String]("ARTIFACTS", O.NotNull)

  def deleted = column[Boolean]("MODULES_DELETED") //TODO: deleted should not be in a module - deletes should be a separate entity because we do not need it for versioning anymore
  def commitHash = column[Option[String]]("MODULES_COMMIT_HASH")
  
  def * = org ~ name ~ version ~ metadata ~ hash ~ artifactHash ~ artifacts ~ depHashes ~ commitHash ~ deleted
  
  //only one hash per commit
  def hashIdx= index("MODULE_HASH_INDEX", (hash, commitHash), unique = true)
  
  def toRow(module: Module, commitHash: Option[Hash], deleted: Boolean)
  :  ModulesType = {
    (module.coords.org, module.coords.name, module.coords.version,  
        module.metadata.toString, module.hash.value,
        module.artifactHash.value,Parsers.setToString(module.artifacts),
        Parsers.setToString(module.deps), 
        commitHash.map(_.value), deleted)
  }
    
        
  def fromRow(t: ModulesType): ModuleRowType = {
    val (org, name, version, metadataString, hashString, artifactHashString, artifactsString, depsString, commitHash, deleted) = t
    
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
    
    (Module(coords, metadata, hash, artifactHash, artifacts,  deps), commitHash, deleted)
  }
}