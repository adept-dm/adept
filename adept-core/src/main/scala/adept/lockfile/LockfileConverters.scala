package adept.lockfile

import adept.artifact.models.ArtifactHash
import adept.artifact.models.ArtifactLocation
import adept.artifact.models.ArtifactAttribute
import play.api.libs.json._
import play.api.libs.functional.syntax._
import adept.repository.models.ResolutionResult

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
    ResolutionResult(id, repository, commit, hash)
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

  def newArtifact(hash: ArtifactHash, size: Int, locations: Set[ArtifactLocation], attributes: Set[ArtifactAttribute], filename: String) = {
    new LockfileArtifact(hash, size, locations.asJava, attributes.asJava, filename)
  }
  
  //....

  private[adept] implicit val formatRequirement: Format[LockfileRequirement] = (
    (
      (__ \ "id").format[String] and
      (__ \ "constraints").format[Map[String, Set[String]]] and
      (__ \ "exclusions").format[Set[String]])({
        case (id, constraints, exclusions) =>
          new LockfileRequirement(
            new Id(id),
            constraints.map { case (name, values) => new Constraint(name, values.asJava) }.toSet.asJava,
            exclusions.map(new Id(_)).asJava)
      },
        unlift({ r: LockfileRequirement =>
          Some((
            r.id.value,
            r.constraints.asScala.map(c => c.name -> c.values.asScala.toSet).toMap,
            r.exclusions.asScala.map(id => id.value).toSet))
        })))

  private[adept] implicit val formatContext: Format[LockfileContext] = (
    (
      (__ \ "info").format[String] and
      (__ \ "id").format[String] and
      (__ \ "repository").format[String] and
      (__ \ "locations").format[Set[String]] and
      (__ \ "commit").format[Option[String]] and
      (__ \ "hash").format[String])({
        case (info, id, repository, locations, commit, variant) =>
          new LockfileContext(
            info,
            new Id(id),
            new RepositoryName(repository),
            locations.map(new RepositoryLocation(_)).asJava,
            commit.map(new Commit(_)).getOrElse(null),
            new VariantHash(variant))
      },
        unlift({ c: LockfileContext =>
          Some((
            c.info,
            c.id.value,
            c.repository.value,
            c.locations.asScala.map(_.value).toSet,
            Option(c.commit).map(_.value),
            c.hash.value))
        })))

  private[adept] implicit val formatArtifact: Format[LockfileArtifact] = (
    (
      (__ \ "hash").format[String] and
      (__ \ "size").format[Int] and
      (__ \ "locations").format[Set[String]] and
      (__ \ "attributes").format[Map[String, Set[String]]] and
      (__ \ "filename").format[Option[String]])({
        case (hash, size, locations, attributes, filename) =>
          new LockfileArtifact(
            new ArtifactHash(hash),
            size,
            locations.map(new ArtifactLocation(_)).asJava,
            attributes.map { case (name, values) => new ArtifactAttribute(name, values.asJava) }.toSet.asJava,
            filename.getOrElse(null))
      },
        unlift({ a: LockfileArtifact =>
          Some((
            a.hash.value,
            a.size,
            a.locations.asScala.map(_.value).toSet,
            a.attributes.asScala.map(c => c.name -> c.values.asScala.toSet).toMap,
            Option(a.filename)))
        })))

  private[adept] implicit val formatLockfile: Format[Lockfile] = (
    (
      (__ \ "requirements").format[Seq[LockfileRequirement]] and
      (__ \ "context").format[Seq[LockfileContext]] and
      (__ \ "artifacts").format[Seq[LockfileArtifact]])({
        case (requirements, context, artifacts) =>
          new Lockfile(
            requirements.toSet.asJava,
            context.toSet.asJava,
            artifacts.toSet.asJava)
      },
        unlift({ lf: Lockfile =>
          Some((
            lf.requirements.asScala.toSeq.sortBy(_.id.value),
            lf.context.asScala.toSeq.sortBy(_.id.value),
            lf.artifacts.asScala.toSeq.sortBy(_.hash.value)))
        })))

  def toJsonString(lockfile: Lockfile) = {
    Json.prettyPrint(Json.toJson(lockfile))
  }


}

