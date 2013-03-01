package adept.core

import java.io.{File => jFile}
import slick.lifted.ForeignKeyAction
import slick.session._

object db {
  lazy val database = {
    Database.forURL("jdbc:h2:"+Configuration.currentAdeptDir()+"/adept", driver = "org.h2.Driver") 
  }

  lazy val driver = slick.driver.H2Driver
    
  lazy val allDDLs = {
    import driver._
    Metadata.ddl ++ Modules.ddl ++ RepositoryMetadata.ddl ++ Dependencies.ddl ++ Artifacts.ddl ++ StagedRepositories.ddl ++ RepositoriesModules.ddl
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

object Metadata extends Table[(String, String, String)]("METADATA") {
  def key = column[String]("KEY", O.NotNull)
  def value = column[String]("VALUE", O.NotNull)
  def moduleHash = column[String]("MODULE_METADATA_HASH", O.NotNull)
  def * = key ~ value ~ moduleHash

  def idx = index("METADATA_INDEX", (moduleHash, key), unique = true)
}

object Dependencies extends Table[(String, String)]("DEPENDENCIES") {
  def parentHash= column[String]("PARENT_HASH", O.NotNull)
  def childHash= column[String]("CHILD_HASH", O.NotNull)
  def * = parentHash ~ childHash
  
  def child = foreignKey("DEP_CHILD_FK", parentHash, Modules)(_.hash, 
      onDelete = ForeignKeyAction.Cascade, onUpdate = ForeignKeyAction.Cascade)
  def parent = foreignKey("DEP_PARENT_FK", parentHash, Modules)(_.hash, 
      onDelete = ForeignKeyAction.Cascade, onUpdate = ForeignKeyAction.Cascade)
}

object Artifacts extends Table[(String, String)]("ARTIFACTS") {
  def location = column[String]("LOCATION", O.NotNull)
  def hash = column[String]("MODULE_HASH", O.NotNull)

  def idx = index("ARTIFACT_INDEX", (location, hash), unique = true)
  
  def hashFk = foreignKey("DEP_FK", hash, Modules)(_.hash, 
      onDelete = ForeignKeyAction.Cascade, onUpdate = ForeignKeyAction.Cascade)
      
  def * = location ~ hash
}

object Modules extends Table[(String, String, String, String)]("MODULES") {
  def hash = column[String]("HASH", O.NotNull)
  def org = column[String]("ORG", O.NotNull)
  def name = column[String]("NAME", O.NotNull)
  def version = column[String]("VERSION", O.NotNull)
  def * = hash ~ org ~ name ~ version
  
  def hashIdx= index("MODULE_HASH_INDEX", hash, unique = true)
  
  def toRow(d: Module): (String, String, String, String) = (d.hash.value, d.coords.org, d.coords.name, d.coords.version)
}

case class Repository(name: String, version: Int) {
  override def toString = s"$name@$version"
}

object StagedRepositories extends Table[(String, Int)]("STAGED_REPOS"){
  def name = column[String]("NAME", O.PrimaryKey)
  def version = column[Int]("VERSION", O.NotNull)
  def * = name ~ version
}

object RepositoriesModules extends Table[(String, Int, String)]("REPOS_MODULES"){
  def name = column[String]("REPOS_MODULES_NAME", O.NotNull)
  def version = column[Int]("REPOS_MODULES_VERSION", O.NotNull)
  def hash = column[String]("REPOS_MODULES_HASH", O.NotNull)
  
  def * = name ~ version ~ hash
}

object RepositoryMetadata extends Table[(String, Int)]("REPOS") {
  def name = column[String]("NAME", O.NotNull)
  def version = column[Int]("VERSION", O.NotNull)
  def * = name ~ version
  
  def nameVersionIdx= index("REPO_NAME_VERSION_INDEX", (name, version), unique = true)
}
