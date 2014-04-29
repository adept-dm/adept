package adept.repository.models

import adept.resolution.models.Id

case class Ranking(id: Id, rankId: RankId, variants: Seq[VariantHash])