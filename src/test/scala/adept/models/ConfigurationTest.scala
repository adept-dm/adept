package adept.models

import org.scalatest.FunSuite
import org.scalatest.MustMatchers

class ConfigurationTest extends FunSuite with MustMatchers {
  val testConf = Configuration("test", Some("this scope indicates that the dependency is not required for normal use of the application, and is only available for the test compilation and execution phases."), Set("runtime"), Visibility.Private, None)

  val configurations = Set(
      Configuration("default", Some("runtime dependencies and master artifact can be used with this conf"), Set("runtime","master"), Visibility.Public, None),
      Configuration("master", Some("contains only the artifact published by this module itself, with no transitive dependencies"), Set.empty, Visibility.Public, None),
      Configuration("compile", Some("this is the default scope, used if none is specified. Compile dependencies are available in all classpaths."), Set.empty, Visibility.Public, None),
      Configuration("provided", Some("this is much like compile, but indicates you expect the JDK or a container to provide it. It is only available on the compilation classpath, and is not transitive."), Set.empty, Visibility.Public, None),
      Configuration("runtime", Some("this scope indicates that the dependency is not required for compilation, but is for execution. It is in the runtime and test classpaths, but not the compile classpath."), Set("compile"), Visibility.Public, None),
      Configuration("sources", Some("this configuration contains the source artifact of this module, if any."), Set.empty, Visibility.Public, None),
      testConf
  )
  
  val modules: Seq[Module] = Seq(
    Module(
        coordinates = Coordinates("org.adept", "adept", "0.1"),
        configurations = configurations,
        dependencies = Set(Dependency(Coordinates("testlib", "testlib", "4.7"), Hash("5d6b71f996d4d8fe96946df191e8151be1609984"), "test->compile(*)"),
                           Dependency(Coordinates("commonlib", "commonlib", "2.0"), Hash("815b9ad4487dd7e4e35a4fe1dc55d9bf86edcf94")),
                           Dependency(Coordinates("extralib", "extralib", "1.0"), Hash("2333da17d8af01d3e45ffdd94b3885ebbfc8ee19"), "runtime->special(master)")),
        artifacts = Set(Artifact(Hash("artihash2"), "jar", Set("compile"), Set("http://url.no/hash2.jar")), Artifact(Hash("artihash-sources"), "jar", Set("sources"), Set("http://url.no/hash2-sources.jar"))),
        attributes = Map("foo_attr" -> Seq("foo_attr"))),
    Module(
        coordinates = Coordinates("testlib", "testlib", "4.7"),
        configurations = configurations,
        dependencies = Set.empty,
        artifacts = Set(Artifact(Hash("artihash3"), "jar", Set("compile"), Set("http://url.no/hash3.jar"))),
        attributes = Map("test" -> Seq("attr"))),      
    Module(
        coordinates = Coordinates("commondeplib", "commondeplib", "1.0"),
        configurations = configurations,
        dependencies = Set.empty,
        artifacts = Set(Artifact(Hash("artihash6"), "jar", Set("compile"), Set("http://url.no/hash6.jar"))),
        attributes = Map("test" -> Seq("attr2"))),      
    Module(
        coordinates = Coordinates("commonlib", "commonlib", "2.0"),
        configurations = configurations,
        dependencies = Set(Dependency(Coordinates("commondeplib", "commondeplib", "1.0"), Hash("0c07ca0548a118c2e57308c641f7b7a3a286a9a1"), "runtime->compile(*)")),
        artifacts = Set(Artifact(Hash("artihash4"), "jar", Set("compile"), Set("http://url.no/hash4.jar"))),
        attributes = Map.empty),
    Module(
        coordinates = Coordinates("extralib", "extralib", "1.0"),
        configurations = configurations,
        dependencies = Set.empty,
        artifacts = Set(Artifact(Hash("artihash5"), "jar", Set("compile"), Set("http://url.no/hash5.jar"))),
        attributes = Map.empty)
  )
    
  test("find extended") {
    Configuration.extendedConfs(configurations, testConf).map(_.name) must be === Set("compile", "runtime")
  }
  
  test("artifact works with basic configurations") {
    Configuration.artifacts(modules(0), "compile")._1.map(_.hash.value).toList.sorted must be === List("artihash2") 
    Configuration.artifacts(modules(0), "test")._2 must have size(2) //visibility of conf 'test' is private
    Configuration.artifacts(modules(0), "strangeconf")._2.map(_.reason) must have size(2) //no conf called strangeconf
  }
  
  test("artifact works with wildcard configurations") {
    Configuration.artifacts(modules(0), "*")._1.map(_.hash) must have size(2)
  }
  
  test("artifact works with multiple configurations") {
    Configuration.artifacts(modules(0), "test;compile;sources")._1.map(_.hash)  must have size(2)
  }
  
  test("dependencies works with configurations") {
    Configuration.modules(modules(0).dependencies, modules, "test")._1.map(p => p._1.coordinates  + " --> " + p._2.map(_.name)) must have size(2) //includes test and compile, but not runtime dependency
  }
  
  test("dependencies works with fallback configurations") {
    val res  = Configuration.modules(modules(0).dependencies, modules, "runtime")  
    res._1.map(p => p._1.hash.value -> p._2.map(_.name).mkString(",")).toList.sorted must be === List( 
        ("2333da17d8af01d3e45ffdd94b3885ebbfc8ee19","master"),  //should return all confs in commonlib
        ("815b9ad4487dd7e4e35a4fe1dc55d9bf86edcf94","provided,master,compile,default,sources,runtime")) //and master from extralib
    res._2 must have size(1) //ignore test scope for testlib
  }
  
  test("recursively find all artifacts for a configuration") {
    val module = modules(0)
    val res = Configuration.allArtifacts(module, modules, "runtime")
    res._1.map(a => a.hash.value -> a.configurations.mkString(",")).toList.sorted must be === List(("artihash2","compile"), ("artihash4","compile"), ("artihash6","compile"))
  }
}