package adept.lockfile

import adept.resolution.models.Id
import adept.resolution.models.Requirement
import adept.resolution.models.Constraint
import adept.repository.models.RepositoryName
import adept.repository.models.VariantHash
import adept.repository.models.Commit
import adept.repository.models.ResolutionResult
import adept.repository.models.RepositoryLocations
import play.api.libs.json._
import play.api.libs.functional.syntax._

case class LockfileRepositoryLocation(value: String)

case class LockfileRequirement(id: Id, constraints: Seq[Constraint], exclusions: Seq[Id], repository: RepositoryName, locations: Seq[LockfileRepositoryLocation], commit: Commit, variant: VariantHash) {
  def toRequirement: Requirement = {
    Requirement(id, constraints.toSet, exclusions.toSet)
  }

  def toResolutionResult: ResolutionResult = {
    ResolutionResult(id, repository, commit, variant)
  }
}

object LockfileRequirement {
  def create(requirement: Requirement, resolutionResult: ResolutionResult, locations: RepositoryLocations) = {
    LockfileRequirement(requirement.id, requirement.constraints.toSeq.sorted, requirement.exclusions.toSeq.sortBy(_.value), resolutionResult.repository, locations.uris.toSeq.map(LockfileRepositoryLocation(_)), resolutionResult.commit, resolutionResult.variant)
  }

  private[adept] implicit val formatLockfileRequirement: Format[LockfileRequirement] = {
    (
      (__ \ "id").format[String] and
      (__ \ "constraints").format[Map[String, Set[String]]] and
      (__ \ "exclusions").format[Seq[String]] and
      (__ \ "repository").format[String] and
      (__ \ "locations").format[Seq[String]] and
      (__ \ "commit").format[String] and
      (__ \ "variant").format[String])({
        case (id, constraints, exclusions, repository, locations, commit, variant) =>
          LockfileRequirement(
            Id(id),
            constraints.map { case (name, values) => Constraint(name, values) }.toSeq,
            exclusions.map(Id(_)).toSeq,
            RepositoryName(repository),
            locations.map(LockfileRepositoryLocation(_)).toSeq,
            Commit(commit),
            VariantHash(variant))
      },
        unlift({ r: LockfileRequirement =>
          val LockfileRequirement(id, constraints, exlusions, repository, locations, commit, variant) = r
          Some((
            id.value,
            constraints.toSeq.sorted.map(c => c.name -> c.values).toMap,
            exlusions.toSeq.map(_.value).sorted,
            repository.value,
            locations.toSeq.map(_.value).sorted,
            commit.value,
            variant.value))
        }))
  }
}
