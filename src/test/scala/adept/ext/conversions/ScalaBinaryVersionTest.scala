package adept.ext.conversions

import org.scalatest._
import org.scalatest.matchers.MustMatchers
import adept.models._
import adept.repository.models._
import adept.repository.models.configuration._
import adept.ext.AttributeDefaults

class ScalaBinaryVersionTest extends FunSuite with MustMatchers {

  test("scala binary version conversion correctness") {
    import OptionValues._
    val akkaName = "akka-actor"
    val scalaVersion = "2.10"
    val configId = Id("config")

    val compileConf = Configuration(
      id = ConfigurationId("compile"),
      extendsConfigurations = Set.empty,
      metadata = Set.empty[MetadataInfo],
      artifacts = Set.empty,
      attributes = Set.empty,
      requirements = Set(ConfiguredRequirement(ScalaBinaryVersion.ScalaLibId, Set(ConfigurationId("compile"), ConfigurationId("master")),
        constraints = Set.empty,
        commit = RepositoryMetadata("repo1", Set.empty, Commit("bogus"), "blahblah")),
        ConfiguredRequirement(configId, Set(ConfigurationId("compile"), ConfigurationId("master")),
          constraints = Set.empty,
          commit = RepositoryMetadata("repo1", Set.empty, Commit("bogus"), "blahblah"))))

    val configuredVariant = ConfiguredVariantsMetadata(
      id = Id(akkaName + "_" + scalaVersion),
      metadata = Set.empty[MetadataInfo],
      attributes = Set(Attribute("version", Set("2.2.1"))),
      configurations = Set(
        compileConf))

    val converted = ScalaBinaryVersion.convert(configuredVariant, Set.empty).value
    converted.id must be === Id(akkaName)
    val configuration = converted.configurations.headOption.value
    configuration.requirements.find(_.id === ScalaBinaryVersion.ScalaLibId).flatMap(_.constraints.find(_.name == AttributeDefaults.BinaryVersionAttribute)) must be === Some(Constraint(AttributeDefaults.BinaryVersionAttribute, Set(scalaVersion)))
    configuration.requirements.find(_.id === configId).flatMap(_.constraints.find(_.name == AttributeDefaults.BinaryVersionAttribute)) must be === None
  }
}