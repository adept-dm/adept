package adept.ext.conversions

import org.scalatest.FunSuite
import org.scalatest.matchers.MustMatchers
import adept.models._
import adept.repository.models._
import adept.repository.models.configuration._

class SemanticVersionTest extends FunSuite with MustMatchers {
  
    val compileConf = Configuration(
    id = ConfigurationId("compile"),
    extendsConfigurations = Set.empty,
    metadata = Set.empty[MetadataInfo],
    artifacts = Set.empty,
    attributes = Set.empty,
    requirements = Set(ConfiguredRequirement(Id("has/binary-version"), Set(ConfigurationId("compile"), ConfigurationId("master")),
      constraints = Set(Constraint("version", Set("2.1.0"))),
      commits = Set.empty),
      ConfiguredRequirement(Id("does/not/have/binary-version"), Set(ConfigurationId("compile"), ConfigurationId("master")),
      constraints = Set(Constraint("version", Set("2.1.0"))),
      commits = Set.empty)))

  val configuredVariants = ConfiguredVariantsMetadata(
    id = Id("master/binary"),
    metadata = Set.empty[MetadataInfo],
    attributes = Set(Attribute("version", Set("1.1.2"))), 
    configurations = Set(
      compileConf))
      
  test("semantic version conversion correctness") {
    pending
  }
}