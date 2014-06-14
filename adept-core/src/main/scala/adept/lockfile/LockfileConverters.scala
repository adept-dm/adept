package adept.lockfile

import adept.artifact.models.ArtifactHash
import adept.artifact.models.ArtifactLocation
import adept.artifact.models.ArtifactAttribute
import adept.repository.models.ContextValue
import adept.services.JsonService
import com.fasterxml.jackson.core.JsonGenerator

private[adept] object LockfileConverters {
  import collection.JavaConverters._

  def asCoreRequirement(lockfileRequirement: LockfileRequirement) = {
    val id = adept.resolution.models.Id(lockfileRequirement.id.value)
    val constraints = Set() ++ lockfileRequirement.constraints.asScala.map { c =>
      adept.resolution.models.Constraint(c.name, Set() ++ c.values.asScala)
    }
    val exclusions = Set() ++ lockfileRequirement.exclusions.asScala.map { id =>
      adept.resolution.models.Id(id.value)
    }
    adept.resolution.models.Requirement(id, constraints, exclusions)
  }

  def asCoreContext(lockfileContext: LockfileContext) = {
    val id = adept.resolution.models.Id(lockfileContext.id.value)
    val commit = Option(lockfileContext.commit).map(c => adept.repository.models.Commit(c.value))
    val repository = adept.repository.models.RepositoryName(lockfileContext.repository.value)
    val hash = adept.repository.models.VariantHash(lockfileContext.hash.value)
    ContextValue(id, repository, commit, hash)
  }

  //Helpers to convert from Scala to Java:
  def create(requirements: Set[LockfileRequirement], context: Set[LockfileContext], artifacts: Set[LockfileArtifact]) = {
    new Lockfile(requirements.asJava, context.asJava, artifacts.asJava)
  }

  def requirements(lockfile: Lockfile) = {
    Set() ++ lockfile.requirements.asScala.map(asCoreRequirement)
  }

  def info(lockfile: Lockfile) = {
    Set() ++ lockfile.context.asScala.map(_.info)
  }

  def context(lockfile: Lockfile) = {
    Set() ++ lockfile.context.asScala.map(asCoreContext)
  }

  def locations(lockfile: Lockfile) = {
    Set() ++ lockfile.context.asScala.map { c =>
      (adept.repository.models.RepositoryName(c.repository.value), adept.resolution.models.Id(c.id.value), Option(c.commit).map(c => adept.repository.models.Commit(c.value)), Set() ++ c.locations.asScala.map(_.value))
    }
  }

  def newContext(info: String, id: adept.resolution.models.Id, repository: adept.repository.models.RepositoryName, locations: Set[String], commit: Option[adept.repository.models.Commit], hash: adept.repository.models.VariantHash) = {
    val commitValue = if (commit.isDefined) commit.get.value else null
    new LockfileContext(info, new Id(id.value), new RepositoryName(repository.value), locations.map(l => new RepositoryLocation(l)).asJava, new Commit(commitValue), new VariantHash(hash.value))
  }

  def newRequirement(id: adept.resolution.models.Id, constraints: Set[adept.resolution.models.Constraint], exclusions: Set[adept.resolution.models.Id]) = {
    new LockfileRequirement(new Id(id.value), constraints.map(c => new Constraint(c.name, c.values.asJava)).asJava, exclusions.map(id => new Id(id.value)).asJava)
  }

  def newArtifact(hash: ArtifactHash, size: Int, locations: Set[ArtifactLocation], attributes: Set[ArtifactAttribute],
                  filename: String) = {
    new LockfileArtifact(hash, size, locations.asJava, attributes.asJava, filename)
  }

  def toJsonString(lockfile: Lockfile) = {
    JsonService.writeJson((generator: JsonGenerator) => {
      val reqs = lockfile.requirements.asScala.toSet
      JsonService.writeArrayField("requirements", reqs, generator)
    })
  }
}

