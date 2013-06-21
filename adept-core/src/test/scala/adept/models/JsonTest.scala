package adept.models

import org.scalatest.FunSuite
import org.scalatest.MustMatchers

class JsonTest extends FunSuite with MustMatchers {
  test("module to json conversion") {
    val coords = Coordinates("org.adept", "adept", "1.0")
    val modules = Seq(
      Module(
        coordinates = coords,
        configurations = Set(Configuration("master", None, Set.empty, Visibility.Public, None), Configuration("test", Some("testing is important"), Set("master"), Visibility.Private, None), Configuration("old-test", Some("this old testing is important"), Set.empty, Visibility.Private, Some("master"))),
        dependencies = Set(Dependency(Coordinates("org.eclipse.jgit", "org.eclipse.jgit", "2.3.1.201302201838-r"), Hash("artihash1"), "master")),
        artifacts = Set(Artifact(Hash("hash1"), "jar", Set("master"), Set("http://url.no/hash1.jar"))),
        attributes = Map("test" -> Seq("attr"))))
    val json = Module.writeJsonForSameCoords(coords, modules)
    val readModules = Module.readSameCoordinates(json)
    readModules.left.foreach(println)
    modules must be === readModules.right.get
  }

}