package adept.ext.conversions

import adept.repository.models.ConfiguredVariantsMetadata

trait Conversion {
  def convert(configuredVariant: ConfiguredVariantsMetadata): Option[ConfiguredVariantsMetadata]
}