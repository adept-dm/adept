package adept.repository.models

import adept.repository.models._
import adept.resolution.models._
import play.api.libs.json._
import play.api.libs.functional.syntax._
import adept.utils.OrderingHelpers

/** The resolution result for each Id: answers the who we found (the variant hash) and where we found it (commit and repository) */
case class ResolutionResult(id: Id, repository: RepositoryName, commit: Commit, variant: VariantHash)

object ResolutionResult {
  implicit val ordering: Ordering[ResolutionResult] = new Ordering[ResolutionResult] {
    def compare(x: ResolutionResult, y: ResolutionResult): Int = {
      if (x.repository.value < y.repository.value)
        -1
      else if (x.repository.value > y.repository.value)
        1
      else {
        if (x.id.value < y.id.value)
          -1
        else if (x.id.value > y.id.value)
          1
        else {
          if (x.commit.value < y.commit.value)
            -1
          else if (x.commit.value > y.commit.value)
            1
          else {
            if (x.variant.value < y.variant.value)
              -1
            else if (x.variant.value > y.variant.value)
              1
            else {
              0
            }
          }
        }
      }
    }
  }
}