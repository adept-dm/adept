package adept.core

import java.io.{File => jFile}
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

case class Artifact(location: String) {
  override def toString = location
}

case class Module(coords: Coordinates, metadata: Metadata, artifactHash: Hash, artifacts: Set[Artifact], hash:Hash, deps: Set[Hash] = Set.empty) {
  val shortString = s"$coords$metadata@$hash" 
  
  override val toString = s"$coords$metadata@$hash$metadata;${artifacts.mkString(",")};${deps.mkString(",")}"
}
case class Metadata(data: Map[String, String]) {
  override val toString = s"[${data.map(e => s"${e._1}=${e._2}")mkString(",")}]" 
}

object Changes {
  val Insert: Option[Boolean] = None
  val Updated: Option[Boolean] = Some(true)
  val Deleted: Option[Boolean] = Some(false)
}

object Modules extends Table[(String, String, String, String, String, String, String, String, String, Int, Option[Boolean])]("MODULES") {
  def hash = column[String]("HASH", O.NotNull)
  def org = column[String]("ORG", O.NotNull)
  def name = column[String]("NAME", O.NotNull)
  def version = column[String]("VERSION", O.NotNull)
  def artifactHash = column[String]("ARTIFACT_HASH", O.NotNull)

  //TODO: I am not sure about representing the ones below as strings instead of tables- it does save space and make it easier wrt versioning:
  def metadata = column[String]("METADATA")
  def childHashes = column[String]("CHILD_HASHES")
  def artifacts = column[String]("ARTIFACTS", O.NotNull)

  //see Changes
  def change = column[Option[Boolean]]("MODULES_CHANGE")
  def repoName = column[String]("MODULES_REPO_NAME", O.NotNull)
  def repoVersion = column[Int]("MODULES_REPO_VERSION", O.NotNull)
  
  def * = hash ~ org ~ name ~ version ~ artifactHash ~ artifacts ~ metadata ~ childHashes ~ repoName ~ repoVersion ~ change
   
  //TODO: add childHashes
  def hashIdx= index("MODULE_HASH_INDEX", (hash, repoName, repoVersion), unique = true)
  
  def setToString(deps: Set[_]) = deps.mkString("[",",","]")
  def setFromString(s: String) = {
    val Expr = """\[(.*?)\]""".r
    s match {
      case Expr(list) => list.split(",").toSet.filter(_.trim.nonEmpty)
      case something => throw new Exception(s"FATAL: could not parse sequence from DB. Got: $s")
    }
  }
  type ModuleType = (String, String, String, String, String, String, String, String, String, Int, Option[Boolean])
  
  def toRow(module: Module, repoName: String, repoVersion: Int, change: Option[Boolean])
  :  ModuleType = 
    (module.hash.value, module.coords.org, module.coords.name, module.coords.version, module.artifactHash.value,
        setToString(module.artifacts), module.metadata.toString, setToString(module.deps), 
        repoName, repoVersion, change)
        
  def fromRow(t: ModuleType) = {
    val (hashString, org, name, version, artifactHashString, artifactsString, metadataString, depsString, repoName, repoVersion, change) = t
    
    val hash = Hash(hashString)
    val artifactHash = Hash(artifactHashString)
    val coords = Coordinates(org, name, version)
    val artifacts = setFromString(artifactsString).map( Artifact.apply )
    val metadata = Parsers.metadata(metadataString).left.map{
      _ => throw new Exception(s"FATAL: could not parse metadata in DB for: $hash. Got: $metadataString")
    }.right.get
    val deps = setFromString(depsString).map( Hash.apply )
    
    (Module(coords, metadata, artifactHash, artifacts, hash,  deps), Repository(repoName, repoVersion))
  }
}

case class Repository(name: String, version: Int) {
  override def toString = s"$name@$version"
}

object RepositoryVersions extends Table[(String, Int, Boolean, Boolean)]("REPOSITORY_VERSIONS") {
  def name = column[String]("NAME", O.NotNull)
  def version = column[Int]("VERSION", O.NotNull)
  def active = column[Boolean]("ACTIVE", O.NotNull)
  def stashed = column[Boolean]("STASHED", O.NotNull)
  def * = name ~ version ~ active ~ stashed
  
  def hashIdx= index("REPOSITORY_UNIQUE_INDEX", (name, version), unique = true)
}
