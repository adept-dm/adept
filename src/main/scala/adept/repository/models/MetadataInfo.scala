package adept.repository.models

/**
 * Metadata information such as license, author names, ...
 * 
 * Use this for information that should be stored but not used for resolution.
 */
case class MetadataInfo(name: String, values: Set[String])