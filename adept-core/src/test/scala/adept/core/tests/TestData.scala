package adept.core.tests

import adept.core.models._

object TestData {

  def findModule(coords: Coordinates, uniqueId: Option[UniqueId], universes: Set[Universe]): Either[Set[Module], Option[Module]] = {
    val all = modules.filter { m =>
      m.coordinates == coords && uniqueId.map(_ == m.uniqueId).getOrElse(true)
    }
    if (all.size > 2) Left(all.toSet)
    else Right(all.headOption)
  }

  val testConf = Configuration("test", Some("this scope indicates that the dependency is not required for normal use of the application, and is only available for the test compilation and execution phases."), Set("runtime"), Visibility.Private, None)
  val defaultConf = Configuration("default", Some("runtime dependencies and master artifact can be used with this conf"), Set("runtime", "master"), Visibility.Public, None)

  lazy val configurations = Set(
    defaultConf,
    Configuration("master", Some("contains only the artifact published by this module itself, with no transitive dependencies"), Set.empty, Visibility.Public, None),
    Configuration("compile", Some("this is the default scope, used if none is specified. Compile dependencies are available in all classpaths."), Set.empty, Visibility.Public, None),
    Configuration("provided", Some("this is much like compile, but indicates you expect the JDK or a container to provide it. It is only available on the compilation classpath, and is not transitive."), Set.empty, Visibility.Public, None),
    Configuration("runtime", Some("this scope indicates that the dependency is not required for compilation, but is for execution. It is in the runtime and test classpaths, but not the compile classpath."), Set("compile"), Visibility.Public, None),
    Configuration("sources", Some("this configuration contains the source artifact of this module, if any."), Set.empty, Visibility.Public, None),
    testConf)

  lazy val testlib47 = Module(
    coordinates = Coordinates("testlib", "testlib", "4.7"),
    uniqueId = UniqueId("testlib-4.7-id"),
    universes = Set.empty,
    configurations = configurations,
    dependencies = Set.empty,
    overrides = Set.empty,
    artifacts = Set(Artifact(Hash("artihash3"), "jar", Set("master"), Set("http://url.no/hash3.jar"))),
    attributes = Map("test" -> Seq("attr")))

  lazy val commonlib20 = Module(
    coordinates = Coordinates("commonlib", "commonlib", "2.0"),
    uniqueId = UniqueId("commonlib-2.0-id"),
    universes = Set.empty,
    configurations = configurations,
    dependencies = Set(
      Dependency(commondeplib20.coordinates, Some(commondeplib20.uniqueId), "compile->compile(*),master(*);runtime->runtime(*)"),
      Dependency(excludedlib10.coordinates, Some(excludedlib10.uniqueId), "compile->compile(*),master(*);runtime->runtime(*)"),
      //Dependency(extralib20.coordinates, Some(extralib20.uniqueId),  "compile->compile(*),master(*);runtime->runtime(*)", force = false),
      Dependency(testlib48.coordinates, Some(testlib48.uniqueId), "test")),
    overrides = Set(Override(extralib20.coordinates.org, extralib20.coordinates.name, extralib20.coordinates.version, None)),
    artifacts = Set(Artifact(Hash("artihash4"), "jar", Set("master"), Set("http://url.no/hash4.jar"))),
    attributes = Map.empty)

  val anotherCommonlib20 = Module(
    coordinates = Coordinates("commonlib", "commonlib", "2.0"),
    uniqueId = UniqueId("commlib-2.0-id2"),
    universes = Set.empty,
    configurations = configurations,
    dependencies = Set(
      Dependency(commondeplib20.coordinates, Some(commondeplib20.uniqueId), "compile->compile(*),master(*);runtime->runtime(*)"),
      Dependency(testlib47.coordinates, Some(testlib47.uniqueId), "test->*")),
    overrides = Set.empty,
    artifacts = Set(Artifact(Hash("artihash4"), "jar", Set("master"), Set("http://url.no/hash4.jar"))),
    attributes = Map.empty)

  lazy val extralib10 = Module(
    coordinates = Coordinates("extralib", "extralib", "1.0"),
    uniqueId = UniqueId("extralib-1.0-id"),
    universes = Set.empty,
    configurations = configurations,
    dependencies = Set.empty,
    overrides = Set.empty,
    artifacts = Set(Artifact(Hash("artihash5"), "jar", Set("master"), Set("http://url.no/hash5.jar"))),
    attributes = Map.empty)

  lazy val extralib20 = Module(
    coordinates = Coordinates("extralib", "extralib", "2.0"),
    uniqueId = UniqueId("extralib-2.0-id"),
    universes = Set.empty,
    configurations = configurations,
    dependencies = Set.empty,
    overrides = Set.empty,
    artifacts = Set(Artifact(Hash("artihash51"), "jar", Set("master"), Set("http://url.no/hash51.jar"))),
    attributes = Map.empty)

  lazy val extralib30 = Module(
    coordinates = Coordinates("extralib", "extralib", "3.0"),
    uniqueId = UniqueId("extralib-3.0-id"),
    universes = Set.empty,
    configurations = configurations,
    dependencies = Set.empty,
    overrides = Set.empty,
    artifacts = Set(Artifact(Hash("artihash51"), "jar", Set("master"), Set("http://url.no/hash51.jar"))),
    attributes = Map.empty)
    
  lazy val extralib40 = Module(
    coordinates = Coordinates("extralib", "extralib", "4.0"),
    uniqueId = UniqueId("extralib-4.0-id"),
    universes = Set.empty,
    configurations = configurations,
    dependencies = Set.empty,
    overrides = Set.empty,
    artifacts = Set(Artifact(Hash("artihash41"), "jar", Set("master"), Set("http://url.no/hash41.jar"))),
    attributes = Map.empty)

  lazy val excludedlib10 = Module(
    coordinates = Coordinates("excludedlib", "excludedlib", "1.0"),
    uniqueId = UniqueId("excludedlib-1.0-id"),
    universes = Set.empty,
    configurations = configurations,
    dependencies = Set.empty,
    overrides = Set.empty,
    artifacts = Set(Artifact(Hash("exluded"), "jar", Set("master"), Set("http://url.no/excluded.jar"))),
    attributes = Map("test" -> Seq("attr")))

  lazy val commondeplib20 = Module(
    coordinates = Coordinates("commondeplib", "commondeplib", "2.0"),
    uniqueId = UniqueId("commondeplib-2.0-id"),
    universes = Set.empty,
    configurations = configurations,
    dependencies = Set.empty,
    overrides = Set.empty,
    artifacts = Set(Artifact(Hash("artihash6"), "jar", Set("master"), Set("http://url.no/hash6.jar"))),
    attributes = Map("test" -> Seq("attr2")))

  lazy val testlib48 = Module(
    coordinates = Coordinates("testlib", "testlib", "4.8"),
    uniqueId = UniqueId("testlib-4.8-id"),
    universes = Set.empty,
    configurations = configurations,
    dependencies = Set.empty,
    overrides = Set.empty,
    artifacts = Set(Artifact(Hash("artihash7"), "jar", Set("master"), Set("http://url.no/hash7.jar"))),
    attributes = Map("test" -> Seq("attr")))

  lazy val adept10 = Module(
    coordinates = Coordinates("org.adept", "adept", "1.0"),
    uniqueId = UniqueId("adept-1.0-id"),
    universes = Set.empty,
    configurations = configurations,
    dependencies = Set(Dependency(testlib47.coordinates, Some(testlib47.uniqueId), "test->default(*)"),
      Dependency(Coordinates("missinglib", "missinglib", "1.0"), None, "*->default(*)"),
      Dependency(commonlib20.coordinates, Some(commonlib20.uniqueId), "*->default(*)", exclusionRules = Set(DependencyExclusionRule("*", "excludedlib"))),
      Dependency(extralib10.coordinates, Some(extralib10.uniqueId), "runtime->special(master)"),
      Dependency(extralib30.coordinates, Some(extralib30.uniqueId), "runtime->special(master)"),
      Dependency(extralib40.coordinates, Some(extralib40.uniqueId), "neverinclude->special(master)")),
    overrides = Set.empty,
    artifacts = Set(Artifact(Hash("artihash2"), "jar", Set("compile"), Set("http://url.no/hash2.jar")), Artifact(Hash("artihash-sources"), "jar", Set("sources"), Set("http://url.no/hash2-sources.jar"))),
    attributes = Map("foo_attr" -> Seq("foo_attr")))

  lazy val adept10Intransitive = Module(
    coordinates = Coordinates("org.adept", "adept", "1.0"),
    uniqueId = UniqueId("adept-1.0-id2"),
    universes = Set.empty,
    configurations = configurations,
    dependencies = Set(Dependency(testlib47.coordinates, Some(testlib47.uniqueId), "test->default(*)"),
      Dependency(Coordinates("missinglib", "missinglib", "1.0"), None, "*->default(*)"),
      Dependency(commonlib20.coordinates, Some(commonlib20.uniqueId), "*->default(*)", isTransitive = false, exclusionRules = Set(DependencyExclusionRule("*", "excludedlib"))),
      Dependency(extralib10.coordinates, Some(extralib10.uniqueId), "runtime->special(master)"),
      Dependency(extralib30.coordinates, Some(extralib30.uniqueId), "runtime->special(master)"),
      Dependency(extralib40.coordinates, Some(extralib40.uniqueId), "neverinclude->special(master)")),
    overrides = Set.empty,
    artifacts = Set(Artifact(Hash("artihash2"), "jar", Set("compile"), Set("http://url.no/hash2.jar")), Artifact(Hash("artihash-sources"), "jar", Set("sources"), Set("http://url.no/hash2-sources.jar"))),
    attributes = Map("foo_attr" -> Seq("foo_attr")))

  lazy val modules: Seq[Module] = Seq(
    adept10,
    testlib47,
    excludedlib10,
    testlib48,
    commondeplib20,
    commonlib20,
    extralib10,
    extralib20,
    extralib30,
    extralib40)
}