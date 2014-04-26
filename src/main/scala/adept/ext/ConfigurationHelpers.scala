package adept.ext

import adept.repository.models.ResolutionResult
import adept.resolution.models.Requirement
import adept.resolution.resolver.models.ResolvedResult
import adept.resolution.Resolver
import adept.resolution.resolver.models.ResolveResult

object ConfigurationHelpers {
  val ConfigurationHashAttribute = "configuration-hash"
  val ConfigurationAttribute = "configuration"
  val ArtifactConfAttribute = "configurations"
  val IdConfig = "config"
}