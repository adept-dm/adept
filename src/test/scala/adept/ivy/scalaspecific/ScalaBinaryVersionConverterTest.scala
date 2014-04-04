package adept.ivy.scalaspecific

import org.scalatest.FunSuite
import org.scalatest.Matchers

import adept.ivy.IvyImportResult
import adept.resolution.models._
import adept.repository.models.RepositoryName
import adept.ext.Version
import adept.ext.AttributeDefaults

class ScalaBinaryVersionConverterTest extends FunSuite with Matchers {
  val scalaLibCompile = Id("org.scala-lang/scala-library/config/compile")
  val scalaLibMaster = Id("org.scala-lang/scala-library/config/master")
  
  test("Basic tests: converting simple results") {
    import ScalaBinaryVersionConverter._
    val result = convertResultWithScalaBinaryVersion(IvyImportResult(
      variant = Variant(
        id = Id("test/thing_2.10"),
        requirements = Set(Requirement(scalaLibCompile, Set.empty, Set.empty),
          Requirement(scalaLibMaster, Set.empty, Set.empty))),
      repository = RepositoryName("testrepo"),
      versionInfo = Set((scalaRepository, scalaLibCompile, Version("2.10.1")),
        (scalaRepository, scalaLibMaster, Version("2.10.1"))),
      //we do not care about these:
      localFiles = Map.empty,
      artifacts = Set.empty,
      excludeRules = Map.empty), { (_: RepositoryName, _: Id) => true })

    result.variant.id shouldEqual Id("test/thing")
    result.variant.requirements shouldEqual Set(Requirement(scalaLibMaster, Set(Constraint(AttributeDefaults.BinaryVersionAttribute, Set("2.10"))), Set.empty), Requirement(scalaLibCompile, Set(Constraint(AttributeDefaults.BinaryVersionAttribute, Set("2.10"))), Set.empty))
  }

  test("Basic tests: converting results with configs") {
import ScalaBinaryVersionConverter._
    val result = convertResultWithScalaBinaryVersion(IvyImportResult(
      variant = Variant(
        id = Id("test/thing_2.10/config/compile"),
        requirements = Set(Requirement(scalaLibCompile, Set.empty, Set.empty), Requirement(Id("foo_2.10/config/compile"), Set.empty, Set.empty), Requirement(Id("zoo/config/compile"), Set.empty, Set.empty))),
      repository = RepositoryName("testrepo"),
      versionInfo = Set((scalaRepository, scalaLibCompile, Version("2.10.1")), (RepositoryName("foobar"), Id("foo_2.10/config/compile"), Version("1.0"))),
      //we do not care about these:
      localFiles = Map.empty,
      artifacts = Set.empty,
      excludeRules = Map.empty), { (_: RepositoryName, _: Id) => true })

    result.variant.id shouldEqual Id("test/thing/config/compile")
    result.variant.requirements shouldEqual Set(Requirement(scalaLibCompile, Set(Constraint(AttributeDefaults.BinaryVersionAttribute, Set("2.10"))), Set.empty), Requirement(Id("foo/config/compile"), Set.empty, Set.empty), Requirement(Id("zoo/config/compile"), Set.empty, Set.empty))
    result.versionInfo shouldEqual Set((scalaRepository, scalaLibCompile, Version("2.10.1")), (RepositoryName("foobar"), Id("foo/config/compile"), Version("1.0"))) //notice the _2.10 is gone
  }
}