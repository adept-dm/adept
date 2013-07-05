package adept.core.tests

import adept.core.models._

object TestData {
  
  def findModule(coords: Coordinates, hash: Option[Hash]): Either[Set[Module], Option[Module]] = {
    val all = modules.filter{ m =>
      m.coordinates == coords && hash.map(_ == m.hash).getOrElse(true)
    }
    if (all.size > 2) Left(all.toSet)
    else Right(all.headOption)
  }
  
  val testConf = Configuration("test", Some("this scope indicates that the dependency is not required for normal use of the application, and is only available for the test compilation and execution phases."), Set("runtime"), Visibility.Private, None)
  val defaultConf = Configuration("default", Some("runtime dependencies and master artifact can be used with this conf"), Set("runtime","master"), Visibility.Public, None)

  val configurations = Set(
      defaultConf,
      Configuration("master", Some("contains only the artifact published by this module itself, with no transitive dependencies"), Set.empty, Visibility.Public, None),
      Configuration("compile", Some("this is the default scope, used if none is specified. Compile dependencies are available in all classpaths."), Set.empty, Visibility.Public, None),
      Configuration("provided", Some("this is much like compile, but indicates you expect the JDK or a container to provide it. It is only available on the compilation classpath, and is not transitive."), Set.empty, Visibility.Public, None),
      Configuration("runtime", Some("this scope indicates that the dependency is not required for compilation, but is for execution. It is in the runtime and test classpaths, but not the compile classpath."), Set("compile"), Visibility.Public, None),
      Configuration("sources", Some("this configuration contains the source artifact of this module, if any."), Set.empty, Visibility.Public, None),
      testConf
  )

  val testlib47 = Module(
        coordinates = Coordinates("testlib", "testlib", "4.7"),
        configurations = configurations,
        dependencies = Set.empty,
        artifacts = Set(Artifact(Hash("artihash3"), "jar", Set("master"), Set("http://url.no/hash3.jar"))),
        attributes = Map("test" -> Seq("attr")))
        
  val commonlib = Module(
        coordinates = Coordinates("commonlib", "commonlib", "2.0"),
        configurations = configurations,
        dependencies = Set(
            Dependency(Coordinates("commondeplib", "commondeplib", "2.0"), Hash("0c07ca0548a118c2e57308c641f7b7a3a286a9a1"), "compile->compile(*),master(*);runtime->runtime(*)"),
            Dependency(Coordinates("excludedlib", "excludedlib", "1.0"), Hash("todo"), "compile->compile(*),master(*);runtime->runtime(*)"),
            Dependency(Coordinates("testlib", "testlib", "4.8"), Hash("a0b9c31e1353e37f07d8bdfedb0fc613a2c2752d"), "test")),
        artifacts = Set(Artifact(Hash("artihash4"), "jar", Set("master"), Set("http://url.no/hash4.jar"))),
        attributes = Map.empty)
        
   val anotherCommonlib = Module(
        coordinates = Coordinates("commonlib", "commonlib", "2.0"),
        configurations = configurations,
        dependencies = Set(
            Dependency(Coordinates("commondeplib", "commondeplib", "2.0"), Hash("0c07ca0548a118c2e57308c641f7b7a3a286a9a1"), "compile->compile(*),master(*);runtime->runtime(*)"),
            Dependency(Coordinates("testlib", "testlib", "4.7"), Hash("todo"), "test->*")),
        artifacts = Set(Artifact(Hash("artihash4"), "jar", Set("master"), Set("http://url.no/hash4.jar"))),
        attributes = Map.empty)
    
  val extralib10 = Module(
        coordinates = Coordinates("extralib", "extralib", "1.0"),
        configurations = configurations,
        dependencies = Set.empty,
        artifacts = Set(Artifact(Hash("artihash5"), "jar", Set("master"), Set("http://url.no/hash5.jar"))),
        attributes = Map.empty)
  
  val extralib20 = Module(
        coordinates = Coordinates("extralib", "extralib", "2.0"),
        configurations = configurations,
        dependencies = Set.empty,
        artifacts = Set(Artifact(Hash("artihash51"), "jar", Set("master"), Set("http://url.no/hash51.jar"))),
        attributes = Map.empty)

  val extralib30 = Module(
        coordinates = Coordinates("extralib", "extralib", "3.0"),
        configurations = configurations,
        dependencies = Set.empty,
        artifacts = Set(Artifact(Hash("artihash51"), "jar", Set("master"), Set("http://url.no/hash51.jar"))),
        attributes = Map.empty)
        
        
  val adept10 = Module(
        coordinates = Coordinates("org.adept", "adept", "1.0"),
        configurations = configurations,
        dependencies = Set(Dependency(Coordinates("testlib", "testlib", "4.7"), Hash("5d6b71f996d4d8fe96946df191e8151be1609984"), "test->default(*)"),
                           Dependency(Coordinates("missinglib", "missinglib", "1.0"), Hash("815b9ad4487dd7e4e35a4fe1dc55d9bf86edcf94"), "*->default(*)"),
                           Dependency(Coordinates("commonlib", "commonlib", "2.0"), Hash("815b9ad4487dd7e4e35a4fe1dc55d9bf86edcf94"), "*->default(*)", exclusionRules = Set(DependencyExclusionRule("*", "excludedlib"))),
                           Dependency(Coordinates("extralib", "extralib", "1.0"), Hash("2333da17d8af01d3e45ffdd94b3885ebbfc8ee19"), "runtime->special(master)"),
                           Dependency(Coordinates("extralib", "extralib", "2.0"), Hash("todo"), "runtime->special(master)"),
                           Dependency(Coordinates("extralib", "extralib", "3.0"), Hash("todo"), "neverinclude->special(master)")),
        artifacts = Set(Artifact(Hash("artihash2"), "jar", Set("compile"), Set("http://url.no/hash2.jar")), Artifact(Hash("artihash-sources"), "jar", Set("sources"), Set("http://url.no/hash2-sources.jar"))),
        attributes = Map("foo_attr" -> Seq("foo_attr")))
 
  val adept10Intransitive = Module(
        coordinates = Coordinates("org.adept", "adept", "1.0"),
        configurations = configurations,
        dependencies = Set(Dependency(Coordinates("testlib", "testlib", "4.7"), Hash("5d6b71f996d4d8fe96946df191e8151be1609984"), "test->default(*)"),
                           Dependency(Coordinates("missinglib", "missinglib", "1.0"), Hash("815b9ad4487dd7e4e35a4fe1dc55d9bf86edcf94"), "*->default(*)"),
                           Dependency(Coordinates("commonlib", "commonlib", "2.0"), Hash("815b9ad4487dd7e4e35a4fe1dc55d9bf86edcf94"), "*->default(*)", isTransitive = false, exclusionRules = Set(DependencyExclusionRule("*", "excludedlib"))),
                           Dependency(Coordinates("extralib", "extralib", "1.0"), Hash("2333da17d8af01d3e45ffdd94b3885ebbfc8ee19"), "runtime->special(master)"),
                           Dependency(Coordinates("extralib", "extralib", "2.0"), Hash("todo"), "runtime->special(master)"),
                           Dependency(Coordinates("extralib", "extralib", "3.0"), Hash("todo"), "neverinclude->special(master)")),
        artifacts = Set(Artifact(Hash("artihash2"), "jar", Set("compile"), Set("http://url.no/hash2.jar")), Artifact(Hash("artihash-sources"), "jar", Set("sources"), Set("http://url.no/hash2-sources.jar"))),
        attributes = Map("foo_attr" -> Seq("foo_attr")))      
        
  val excluded = Module(
        coordinates = Coordinates("excludedlib", "excludedlib", "1.0"),
        configurations = configurations,
        dependencies = Set.empty,
        artifacts = Set(Artifact(Hash("exluded"), "jar", Set("master"), Set("http://url.no/excluded.jar"))),
        attributes = Map("test" -> Seq("attr")))

  val modules: Seq[Module] = Seq(
    adept10,
    testlib47,
    excluded,
    Module(
        coordinates = Coordinates("testlib", "testlib", "4.8"),
        configurations = configurations,
        dependencies = Set.empty,
        artifacts = Set(Artifact(Hash("artihash7"), "jar", Set("master"), Set("http://url.no/hash7.jar"))),
        attributes = Map("test" -> Seq("attr"))),   
    Module(
        coordinates = Coordinates("commondeplib", "commondeplib", "2.0"),
        configurations = configurations,
        dependencies = Set.empty,
        artifacts = Set(Artifact(Hash("artihash6"), "jar", Set("master"), Set("http://url.no/hash6.jar"))),
        attributes = Map("test" -> Seq("attr2"))),      
    commonlib,
    extralib10,
    extralib20,
    extralib30
  )
}