package adept.core.models

/** The super class for classes that describes which dependency to choose */
class DependencyDescriptor(val organization: String, val name: String, val preferredVersion: String, val uniqueId: Option[UniqueId]) {
  def asCoordinates = Coordinates(organization, name, preferredVersion)
}
