package adept.ext.conversions

import adept.repository.models.ConfiguredVariantsMetadata

trait Conversion {
  def convert(configuredVariant: ConfiguredVariantsMetadata, others: Set[ConfiguredVariantsMetadata]): Option[ConfiguredVariantsMetadata]
}