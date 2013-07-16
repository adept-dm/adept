package adept.core.models

import org.scalatest.FunSuite
import org.scalatest.MustMatchers

class JsonTest extends FunSuite with MustMatchers {
  test("module to json conversion") {
    import org.scalatest.EitherValues._
    val coords = Coordinates("org.adept", "adept", "1.0")
    val modules = Seq(
      Module(
        coordinates = coords,
        uniqueId = UniqueId("foo-bar-adept-1.0-1231232123-adfas"),
        configurations = Set(Configuration("master", None, Set.empty, Visibility.Public, None), Configuration("test", Some("testing is important"), Set("master"), Visibility.Private, None), Configuration("old-test", Some("this old testing is important"), Set.empty, Visibility.Private, Some("master"))),
        dependencies = Set(Dependency(Coordinates("org.eclipse.jgit", "org.eclipse.jgit", "2.3.1.201302201838-r"), Some(UniqueId("jgit-id")), "master", isTransitive = true, exclusionRules = Set(DependencyExclusionRule("*", "foo"))),
            Dependency(Coordinates("something.else", "module1", "1.0"), Some(UniqueId("module1-id")), "default->*", force = true, isTransitive = false)),
        overrides = Set(Override("foo", "bar", "1.0", None), Override("foo", "bar", "1.0", Some(UniqueId("foo")))),
        artifacts = Set(Artifact(Hash("hash1"), "jar", Set("master"), Set("http://url.no/hash1.jar"))),
        attributes = Map("test" -> Seq("attr"))))
    val json = Module.writeJsonForSameCoords(coords, modules)
    val readModules = Module.readSameCoordinates(json)
      
    import org.json4s.native.JsonMethods._
    println(pretty(render(json)))
    modules must be === readModules.right.value
  }

}