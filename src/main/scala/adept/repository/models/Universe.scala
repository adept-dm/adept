package adept.repository.models

import adept.models.Hash

case class UniverseId(hashes: Set[Hash])

case class Universe(order: Seq[UniverseId]) 