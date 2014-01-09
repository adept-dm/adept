package adept.configuration

import adept.core.models._

object ConfiguredDependency {
  private val ConfigPattern = (Id.Sep + VariantBuilder.ConfigIdValue + Id.Sep + "(.*?)$").r
}

case class ConfiguredDependency(id: Id, configurations: Set[ConfigurationId], constraints: Set[Constraint]) {
  def toDependencies: Set[Dependency] = {
    configurations.map { configuration =>
      Dependency(VariantBuilder.mapId(id, configuration), constraints)
    }
  }
}