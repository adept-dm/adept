package adept.core.models

import java.io.File

//TODO: private[core]
case class Module(coords: Coordinates, metadata: Metadata, hash:Hash,  artifactHash: Hash, artifacts: Set[Artifact], deps: Set[Hash] = Set.empty) {
  override def toString = s"$coords$metadata@$hash#$artifactHash!${artifacts.mkString(",")}%${deps.mkString(",")}"
}

object Module{
  def fromFile(jarFile: File, coords: Coordinates, metadata: Metadata, artifacts: Set[Artifact], deps: Set[Hash] = Set.empty): Module = {
    val artifactHash = Hash.calculate(jarFile)
    val hash = Hash.calculate(Seq(artifactHash, Hash(coords.toString)))
    Module(coords, metadata, hash, artifactHash, artifacts, deps)
  }
}