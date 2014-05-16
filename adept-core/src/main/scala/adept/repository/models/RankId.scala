package adept.repository.models

case class RankId(value: String) extends AnyVal

object RankId {
  implicit val ordering =new Ordering[RankId] {
    def compare(x: RankId, y: RankId): Int = {
      x.value.compare(y.value)
    }
  }
}