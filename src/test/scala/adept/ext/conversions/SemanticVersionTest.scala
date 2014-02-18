package adept.ext.conversions

import org.scalatest._
import org.scalatest.matchers.MustMatchers
import adept.models._
import adept.repository.models._
import adept.repository.models.configuration._
import adept.ext.AttributeDefaults

class SemanticVersionTest extends FunSuite with MustMatchers {

  test("semantic version conversion correctness") {
//    val binaryVersionReqId = Id("has/binary-version")
//    val noBinaryVersionReqId = Id("does/not/have/binary-version")
//    val masterId = Id("master/binary")
//
//    val compileConf = Configuration(
//      id = ConfigurationId("compile"),
//      extendsConfigurations = Set.empty,
//      metadata = Set.empty[MetadataInfo],
//      artifacts = Set.empty,
//      attributes = Set.empty,
//      requirements = Set(
//        ConfiguredRequirement(binaryVersionReqId, Set(ConfigurationId("compile"), ConfigurationId("master")),
//          constraints = Set(Constraint("version", Set("2.1.0")))),
//        ConfiguredRequirement(noBinaryVersionReqId, Set(ConfigurationId("compile"), ConfigurationId("master")),
//          constraints = Set(Constraint("version", Set("2.1.0"))))))
//
//    val configuredVariant = ConfiguredVariantsMetadata(
//      id = masterId,
//      metadata = Set.empty[MetadataInfo],
//      attributes = Set(Attribute("version", Set("1.1.2"))),
//      configurations = Set(
//        compileConf))
//
//    import OptionValues._
//    val sematicVersionConversion = new SemanticVersion(Set(binaryVersionReqId, masterId))
//    val converted = sematicVersionConversion.convert(configuredVariant, Set.empty).value
//
//    val config = converted.configurations.headOption.value
//    config.requirements.find(_.id == binaryVersionReqId).flatMap(_.constraints.find(_.name == AttributeDefaults.BinaryVersionAttribute)) must be === Some(Constraint(AttributeDefaults.BinaryVersionAttribute, Set("2.1")))
//    config.requirements.find(_.id == noBinaryVersionReqId).flatMap(_.constraints.find(_.name == AttributeDefaults.BinaryVersionAttribute)) must be === None
//    converted.attributes.find(_.name === AttributeDefaults.BinaryVersionAttribute) must be === Some(Attribute(AttributeDefaults.BinaryVersionAttribute, Set("1.1")))
    pending //TODO: fix failing test
  }
}