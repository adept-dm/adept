package adept.repository.models

import adept.repository.models._
import adept.resolution.models._
import play.api.libs.json._
import play.api.libs.functional.syntax._
import adept.utils.OrderingHelpers

case class RepositoryInfo(id: Id, repository: RepositoryName, commit: Commit, variants: VariantSet)

//object RepositoryInfo {
//  implicit val ordering: Ordering[RepositoryInfo] = new Ordering[RepositoryInfo] {
//    def compare(x: RepositoryInfo, y: RepositoryInfo): Int = {
//      if (x.id.value < y.id.value) -1
//      else if (x.id.value > y.id.value) 1
//      else {
//        assert(x.id.value == y.id.value)
//        if (x.repository.value < y.repository.value) -1
//        else if (x.repository.value > y.repository.value) 1
//        else {
//          assert(x.repository.value == y.repository.value)
//          if (x.commit.value < y.commit.value) -1
//          else if (x.commit.value > y.commit.value) 1
//          else {
//            assert(x.commit.value == y.commit.value)
//            OrderingHelpers.stringSetCompare(x.variants.hashes.map(_.value), y.variants.hashes.map(_.value))
//          }
//        }
//      }
//    }
//  }
//}
