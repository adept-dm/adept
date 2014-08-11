package adept.repository.metadata

import java.io.{File, FileFilter, InputStream}

import adept.hash.Hasher
import adept.repository.{GitRepository, Repository}
import adept.repository.models._
import adept.resolution.models._
import adept.services.JsonService
import com.fasterxml.jackson.core.JsonGenerator

case class VariantMetadata(attributes: Seq[Attribute], artifacts: Seq[ArtifactRef],
                           requirements: Seq[Requirement]) {

  def toVariant(id: Id): Variant = {
    Variant(id, attributes.toSet, artifacts.toSet, requirements.toSet)
  }

  lazy val hash: VariantHash = {
    // This means we are whitespace sensitive, on the other hand it makes it easier to create other tools
    // that generate/checks the hash (SHA-256 of the contents).
    // We are also assuming that there is not that many people who will actually edit the content manually,
    // and that if they do it will be picked up by Git so it is very visible.
    // Adept only reads from Git so we again we should be good.
    // I agree that it feels dangerous (and not very defensive), but it is better than using another string
    // as basis for the hash (without whitespaces) because that would be even more confusing and even
    // harder to define exactly how the hash is calculated.
    // Having a hash that is calculated from the internals would be a more sensible option, but it makes it
    // hard to define how the hash should be. You could imagine that 2 platforms for example sort strings
    // differently which would make it hard.
    VariantHash(Hasher.hash(jsonString.getBytes))
  }

  lazy val jsonString = {
    JsonService.writeJson({generator: JsonGenerator =>
      JsonService.writeArrayField("attributes", attributes, generator)
      JsonService.writeArrayField("artifacts", artifacts, generator)
      JsonService.writeArrayField("requirements", requirements, generator)
    })
  }

  def write(id: Id, repository: Repository): File = {
    require(hash.value.length == Repository.HashLength, "Hash for: " + id +
      " (" + this + ") has length:"
      + hash.value.length + " but should have " + Repository.HashLength)
    val file = repository.ensureVariantFile(id, hash)
    MetadataContent.write(jsonString, file)
  }
}

object VariantMetadata {

  def fromVariant(variant: Variant): VariantMetadata = {
    VariantMetadata(variant.attributes.toSeq, variant.artifacts.toSeq, variant.requirements.toSeq)
  }

  private def readJson(id: Id, hash: VariantHash, repository: Repository, is: InputStream,
                       checkHash: Boolean):
  Option[VariantMetadata] = {
    val value = JsonService.parseJson(is, Map(
        ("attributes", JsonService.parseSeq(_, Attribute.fromJson)),
        ("artifacts", JsonService.parseSeq(_, ArtifactRef.fromJson)),
        ("requirements", JsonService.parseSeq(_, Requirement.fromJson))
      ), valueMap => VariantMetadata(valueMap.getSeq[Attribute]("attributes"),
            valueMap.getSeq[ArtifactRef]("artifacts"), valueMap.getSeq[Requirement](
        "requirements"))
    )._1
    if (checkHash) {
      if (value.hash == hash) {
        Some(value)
      } else {
        throw new Exception("Found variant metadata: " + value + " for hash " + hash +
          " but it has a different hash: " + value.hash)
      } //TODO: this might be overkill?
    } else Some(value)
  }

  def read(id: Id, hash: VariantHash, repository: Repository, checkHash: Boolean):
  Option[VariantMetadata] = {
    val file = repository.getVariantFile(id, hash)
    repository.usingFileInputStream(file) {
      case Right(Some(is)) =>
        readJson(id, hash, repository, is, checkHash)
      case Right(None) => None
      case Left(error) =>
        throw new Exception("Could not read file: " + file.getAbsolutePath + " for id: " + id +
          " hash: " + hash + ". Got error: " + error)
    }
  }

  def read(id: Id, hash: VariantHash, repository: GitRepository, commit: Commit, checkHash:
  Boolean = true):
  Option[VariantMetadata] = {
    repository.usingVariantInputStream(id, hash, commit) {
      case Right(Some(is)) =>
        readJson(id, hash, repository, is, checkHash)
      case Right(None) => None
      case Left(error) =>
        throw new Exception("Could not read: " + hash + " for commit: " + commit + " in dir:  " +
          repository.dir + ". Got error: " + error)
    }
  }

  import adept.repository.GitRepository._
  import adept.repository.Repository._

  private[adept] val HashExtractionRegex = {
    s"""$VariantsMetadataDirName$GitPathSep(.*)$GitPathSep([0-9a-fA-F]{${Repository.Level1Length}})$GitPathSep([0-9a-fA-F]{${Repository.Level2Length}})$GitPathSep([0-9a-fA-F]{${Repository.Level3Length}})$GitPathSep$VariantMetadataFileName""".r
  }

  private[adept] val Level1FileFilter = new FileFilter() {
    override def accept(file: File) = {
      file.isDirectory && file.getName.size == Repository.Level1Length
    }
  }

  private[adept] val Level2FileFilter = new FileFilter() {
    override def accept(file: File) = {
      file.isDirectory && file.getName.size == Repository.Level2Length
    }
  }

  private[adept] val Level3FileFilter = new FileFilter() {
    override def accept(file: File) = {
      file.isDirectory && file.getName.size == Repository.Level3Length
    }
  }

  private[adept] val VariantFileFilter = new FileFilter() {
    override def accept(file: File) = {
      file.isFile && file.getName == Repository.VariantMetadataFileName
    }
  }

  def listVariants(id: Id, repository: Repository): Set[VariantHash] = {
    val idDir = new File(Repository.getVariantsMetadataDir(repository.baseDir, repository.name), id.value)
    idDir.listFiles(Level1FileFilter).flatMap { level1Dir =>
      level1Dir.listFiles(Level2FileFilter).flatMap { level2Dir =>
        level2Dir.listFiles(Level3FileFilter).flatMap { level3Dir =>
          if ((level1Dir.getName + level2Dir.getName + level3Dir.getName).size ==
            Repository.HashLength) {
            level3Dir.listFiles(VariantFileFilter).flatMap { variantFile =>
              Some(VariantHash(level1Dir.getName + level2Dir.getName + level3Dir.getName))
            }
          } else Array.empty[VariantHash]
        }
      }
    }.toSet
  }

  def listVariants(id: Id, repository: GitRepository, commit: Commit): Set[VariantHash] = {
    repository.usePath[VariantHash](Some(VariantsMetadataDirName), commit) {
      case HashExtractionRegex(idValue, level1, level2, level3) if idValue == id.value =>
        Some(VariantHash(level1 + level2 + level3))
      case _ => None
    }
  }

  def listIds(repository: Repository): Set[Id] = {
    RankingMetadata.findRankingFiles(repository).map { rankingFile =>
      val IdRankingExtractor = (Repository.getVariantsMetadataDir(repository.baseDir,
        repository.name).getAbsolutePath + "/(.*?)/" + rankingFile.getName).r
      val IdRankingExtractor(idString) = rankingFile.getAbsolutePath
      Id(idString)
    }.toSet
  }

  def listIds(repository: GitRepository, commit: Commit): Set[Id] = {
    val IdExtractionRegex = s"""$VariantsMetadataDirName$GitPathSep(.*)$GitPathSep(.*?)$GitPathSep(.*?)$GitPathSep(.*?)$GitPathSep$VariantMetadataFileName""".r
    repository.usePath[Id](Some(VariantsMetadataDirName), commit) {
      case IdExtractionRegex(id, level1, level2, level3) =>
        Some(Id(id))
      case _ => None
    }
  }

}
