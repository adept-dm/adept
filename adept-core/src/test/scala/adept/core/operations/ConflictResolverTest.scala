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
    /* should look like this?
org.adept:adept:1.0 (compile)
  \___ overridden dependencies
      @___ commonlib:commonlib:1.9 (overridden: overridden by: commonlib:commonlib:1.9 in override:module:1.0)
      @___ commondeplib:commondeplib:1.9 (overridden: overridden by: commondeplib:commondeplib:1.9 in commonlib:commonlib:1.9)
  V___ commondeplib:commondeplib:1.9 (compile)
    \___ artifacts
        X___ artihash6.1 (evicted: could not find any configurations for 'master' from compile)
  V___ override:module:1.0 (default)
  V___ commonlib:commonlib:1.9 (compile)
    \___ artifacts
        X___ artihash4.1 (evicted: could not find any configurations for 'master' from compile)
    V___ excludedlib:excludedlib:1.0 (compile,master)
      \___ artifacts
          V___ exluded (master)
    V___ commondeplib:commondeplib:1.9 (compile,master)
      \___ artifacts
          V___ artihash6.1 (master)
    X___ testlib:testlib:4.7 (evicted: no matching configurations: expression 'test' does not match confs: compile)
  X___ commondeplib:commondeplib:2.0 (evicted: overridden by: commondeplib:commondeplib:1.9 in commonlib:commonlib:1.9)
  X___ commonlib:commonlib:2.0 (evicted: overridden by: commonlib:commonlib:1.9 in override:module:1.0)
     */
    println(tree)
    pending
  }
}