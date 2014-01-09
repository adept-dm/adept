package adept.configuration

import adept.core.models._

class ConfiguredVariantInfo private[adept](val artifacts: Set[ArtifactRef], val dependencies: Set[ConfiguredDependency], val configuration: ConfigurationId, val extendsConfigurations: Set[ConfigurationId], val description: String)

class VariantBuilder(id: Id, attributes: Set[Attribute], configuredVariants: Seq[ConfiguredVariantInfo] = IndexedSeq.empty) {
  def addConfiguration(artifacts: Set[ArtifactRef], dependencies: Set[ConfiguredDependency], configuration: ConfigurationId, extendsConfigurations: Set[ConfigurationId], description: String = ""): VariantBuilder = {
    new VariantBuilder(id, attributes, configuredVariants :+ new ConfiguredVariantInfo(artifacts, dependencies, configuration, extendsConfigurations, description))
  }
  
  def +(configuredVariantInfo: ConfiguredVariantInfo) = configuredVariants :+ configuredVariantInfo
  
  def build(): Set[Variant] = {
    if (id.value.isEmpty) throw new Exception("Cannot build a variant with an empty Id. Attributes: " + attributes.mkString(","))

    val configureHash = Hash.calculate(attributes, configuredVariants.map(_.configuration).toSet)
    val configureAttribute = Attribute(VariantBuilder.ConfigHashAttributeName, Set(configureHash.value))

    val variants: Set[Variant] = configuredVariants.map { configuredVariantInfo =>
      val configurationDependencies = configuredVariantInfo.extendsConfigurations.map { configuration => //TODO: check if configuration are cyclic - they should never be cyclic
        Dependency(VariantBuilder.mapId(id, configuration), Set(Constraint(VariantBuilder.ConfigHashAttributeName, Set(configureHash.value))))
      } + Dependency(id, Set(Constraint(VariantBuilder.ConfigHashAttributeName, Set(configureHash.value))))
      Variant(VariantBuilder.mapId(id, configuredVariantInfo.configuration), configuredVariantInfo.artifacts, attributes + configureAttribute, configuredVariantInfo.dependencies.flatMap(_.toDependencies) ++ configurationDependencies, Set(VariantBuilder.ConfigDescriptionAttributeName -> configuredVariantInfo.description))
    }.toSet
    variants + Variant(id, artifacts = Set.empty, attributes = attributes + configureAttribute, dependencies = Set.empty) //base variant to enforce that only configurations of this exact variant are used 
  }
}

/**
 * Usage:
 *
 * {{{
 * VariantBuilder.create(Id("A"), attributes, artifacts, dependencies, ConfigurationId("compile"))
 *  .addConfiguration(artifacts, dependencies, ConfigurationId("test"), Set(ConfigurationId("compile")), "test configurations")
 *  .build()
 * }}}
 */
object VariantBuilder {
  val ConfigIdValue = "config"
  val ConfigHashAttributeName = "configuration-hash"
  val ConfigDescriptionAttributeName = "configuration-description"

  def mapId(id: Id, configId: ConfigurationId) = {
    Id(id.value + Id.Sep + ConfigIdValue + Id.Sep + configId.value)
  }

  def create(id: Id, attributes: Set[Attribute]): VariantBuilder = {
    new VariantBuilder(id, attributes)
  }

  def create(id: Id, attributes: Set[Attribute], artifacts: Set[ArtifactRef], dependencies: Set[ConfiguredDependency], configuration: ConfigurationId, extendsConfigurations: Set[ConfigurationId] = Set.empty, description: String = ""): VariantBuilder = {
    create(id, attributes).addConfiguration(artifacts, dependencies, configuration, extendsConfigurations, description)
  }
}