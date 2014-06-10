package adept.repository.models

import adept.repository.models._
import adept.resolution.models._
import play.api.libs.json._
import play.api.libs.functional.syntax._
import adept.utils.OrderingHelpers

/** The context value for each Id: answers the who we found (the variant hash) and where we found it (commit and repository) */
case class ContextValue(id: Id, repository: RepositoryName, commit: Option[Commit], variant: VariantHash) //TODO: rename variant to hash

object ContextValue {
  implicit val ordering: Ordering[ContextValue] = new Ordering[ContextValue] {
    def compare(x: ContextValue, y: ContextValue): Int = {
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
          if (x.commit.isDefined && y.commit.isDefined) {
            val xcommit = x.commit.get
            val ycommit = y.commit.get
            if (xcommit.value < ycommit.value)
              -1
            else if (xcommit.value > ycommit.value)
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
          } else {
            if (x.commit.isDefined && !y.commit.isDefined) {
              1
            } else {
              -1
            } 
          }
        }
      }
    }
  }
}
