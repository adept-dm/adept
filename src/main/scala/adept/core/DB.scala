package adept.core

import java.io.{File => jFile}
import slick.lifted.ForeignKeyAction
import slick.session.Database

object db {
  lazy val database = {
    Database.forURL("jdbc:h2:"+Configuration.currentAdeptDir()+"/adept", driver = "org.h2.Driver") 
  }

  lazy val driver = slick.driver.H2Driver
    
  lazy val allDDLs = {
    import driver._
    Metadata.ddl ++ Modules.ddl ++ RepositoryMetadata.ddl ++ Dependencies.ddl
  }
}
import db.driver.simple._

case class Coordinates(org: String, name: String, version: String) {
  override val toString = s"$org:$name:$version" 
}
case class Hash(value: String) {
  override val toString = value 
}
object Hash {
  private lazy val md = java.security.MessageDigest.getInstance("SHA-1")
  private def encode(bytes: Array[Byte]) = {
    md.digest(bytes).map(b => "%02X" format b).mkString.toLowerCase
  }
  
  def calculate(coords: Coordinates, jarFile: jFile): Hash = {
    val jarSource = io.Source.fromFile(jarFile)
    val hash = try {
      encode(jarSource.map(_.toByte).toArray ++ (coords.org + coords.name + coords.version).getBytes)
    } finally {
      jarSource.close()
    }
    Hash(hash)
  }
}
case class Module(coords: Coordinates, metadata: Metadata, hash:Hash) {
  override val toString = s"$coords$metadata@$hash" 
}
case class Metadata(data: Map[String, String]) {
  def addScalaVersion(version: String): Metadata = { //TODO: as long we do not need it
    this.copy(data = data + ("scala-version" -> version))
  }
  override val toString = s"[${data.map(e => s"${e._1}=${e._2}")mkString(",")}]" 
}

object Metadata extends Table[(Int, String, String, String)]("METADATA") {
  def id = column[Int]("METADATA_ID", O.PrimaryKey, O.AutoInc)
  def key = column[String]("KEY", O.NotNull)
  def value = column[String]("VALUE", O.NotNull)
  def moduleHash = column[String]("MODULE_METADATA_HASH", O.NotNull)
  def * = id ~ key ~ value ~ moduleHash

  def idx = index("METADATA_INDEX", (moduleHash, key), unique = true)
  def autoInc = key ~ value ~ moduleHash returning id 
}

object Dependencies extends Table[(Int, String, String)]("DEPENDENCIES") {
  def id = column[Int]("DEP_ID", O.PrimaryKey, O.AutoInc)
  def parentHash= column[String]("PARENT_HASH", O.NotNull)
  def childHash= column[String]("CHILD_HASH", O.NotNull)
  def * = id ~ parentHash ~ childHash
  
  def child = foreignKey("DEP_PARENT_FK", parentHash, Modules)(_.hash, 
      onDelete = ForeignKeyAction.Cascade, onUpdate = ForeignKeyAction.Cascade)
  def parent = foreignKey("DEP_CHILD_FK", parentHash, Modules)(_.hash, 
      onDelete = ForeignKeyAction.Cascade, onUpdate = ForeignKeyAction.Cascade)
      
  def autoInc = parentHash ~ childHash returning id
}

object Modules extends Table[(Int, String, String, String, String, Int)]("MODULES") {
  def id = column[Int]("MODULE_ID", O.AutoInc, O.PrimaryKey)
  def hash = column[String]("HASH", O.NotNull)
  def org = column[String]("ORG", O.NotNull)
  def name = column[String]("NAME", O.NotNull)
  def version = column[String]("VERSION", O.NotNull)
  def repositoryMetadata = column[Int]("REPOSITORY_METADATA", O.NotNull)
  
  def hashIdx= index("MODULE_HASH_INDEX", hash, unique = true)
  
  def * = id ~ hash ~ org ~ name ~ version ~ repositoryMetadata
  
  def autoInc = hash ~ org ~ name ~ version ~ repositoryMetadata returning id
  
  def toRow(d: Module, repoId: Int): (String, String, String, String, Int) = (d.hash.value, d.coords.org, d.coords.name, d.coords.version, repoId)
}

object RepositoryMetadata extends Table[(Int, String)]("REPOSITORY") {
  def version = column[Int]("VERSION", O.PrimaryKey, O.AutoInc)
  def name = column[String]("NAME", O.NotNull)
  def * = version ~ name
  
  def autoInc = name returning version
}
