package adept.core.operations

import org.scalatest._
import adept.core.operations._
import adept.core.models._

class ConflictResolverTest extends FunSuite with MustMatchers {
  import adept.core.tests.TestData._

  test("basic conflict resolution") {
    import org.scalatest.OptionValues._
    val tree = TreeOperations.build("test", adept10, Configuration.defaultConfigurationMapping(_), findModule(modules)).value
    ConflictResolver.resolveConflicts(tree, Configuration.defaultConfigurationMapping(_), findModule(modules))
    //println(tree) //TODO
    pending
  }

  test("nested overrides") {
    import org.scalatest.OptionValues._
    val overrideModule10 = Module(
      coordinates = Coordinates("override", "module", "1.0"),
      uniqueId = UniqueId("adept-1.0-id"),
      universes = Set.empty,
      configurations = configurations,
      dependencies = Set.empty,
      overrides = Set(Override(commonlib19.coordinates.org, commonlib19.coordinates.name, commonlib19.coordinates.version, None)),
      artifacts = Set.empty,
      attributes = Map.empty)

    val overrideRoot = Module(
      coordinates = Coordinates("org.adept", "adept", "1.0"),
      uniqueId = UniqueId("adept-1.0-id"),
      universes = Set.empty,
      configurations = configurations,
      dependencies = Set(
        Dependency(commondeplib20.coordinates, Some(commondeplib20.uniqueId), "compile->compile(*),master(*);runtime->runtime(*)"),
        Dependency(overrideModule10.coordinates, Some(overrideModule10.uniqueId), "compile->default(*)"),
        Dependency(commonlib20.coordinates, Some(commonlib20.uniqueId), "*->default(*)", exclusionRules = Set(DependencyExclusionRule("*", "excludedlib")))),
      overrides = Set.empty,
      artifacts = Set.empty,
      attributes = Map.empty)

    val tree = TreeOperations.build("compile", overrideRoot, Configuration.defaultConfigurationMapping(_), findModule(modules ++ Seq(overrideRoot, overrideModule10))).value
    ConflictResolver.resolveConflicts(tree, Configuration.defaultConfigurationMapping(_), findModule(modules))
    println(tree)
    pending
  }
}