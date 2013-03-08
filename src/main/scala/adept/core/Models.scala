package adept.core

import java.io.{File => jFile}

case class Module(coords: Coordinates, metadata: Metadata, artifactHash: Hash, artifacts: Set[Artifact], hash:Hash, deps: Set[Hash] = Set.empty) {
  val shortString = s"$coords$metadata@$hash" 
  
  override val toString = s"$coords$metadata@$hash$metadata;${artifacts.mkString(",")};${deps.mkString(",")}"
}

case class Artifact(location: String) {
  override def toString = location
}

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

case class Metadata(data: Map[String, String]) {
  override val toString = s"[${data.map(e => s"${e._1}=${e._2}")mkString(",")}]" 
}

case class Repository(name: String, version: Int) {
  override def toString = s"$name@$version"
}

case class ChangeSet(repo: Repository, moduleChanges: Seq[(Module, Boolean)])



