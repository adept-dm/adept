package adept.repository.models

import org.scalatest.FunSuite
import org.scalatest.matchers.MustMatchers
import adept.models._
import adept.repository.models.configuration._
import java.io.StringWriter
import java.io.StringReader

object ConfiguredVariantsMetadataTestData {

  val compileConf = Configuration(
    id = ConfigurationId("compile"),
    extendsConfigurations = Set.empty,
    metadata = Set(MetadataInfo("configuration-description", Set("this is a compile conf"))),
    artifacts = Set(ArtifactRef(Hash("12345"), Set(Attribute("configuration", Set("master"))), Some("filename.jar"))),
    attributes = Set(Attribute("attr1", Set("foo"))),
    requirements = Set(ConfiguredRequirement(Id("mooboo"), Set(ConfigurationId("compile"), ConfigurationId("master")),
      constraints = Set(Constraint("version", Set("1.0.0"))))))

  val runtimeConf = Configuration(
    id = ConfigurationId("runtime"),
    extendsConfigurations = Set(ConfigurationId("compile")),
    metadata = Set(MetadataInfo("configuration-description", Set("this is a runtime conf"))),
    artifacts = Set.empty,
    attributes = Set(Attribute("attr2", Set("foo"))),
    requirements = Set.empty)

  val testConf = Configuration(
    id = ConfigurationId("test"),
    extendsConfigurations = Set(ConfigurationId("compile"), ConfigurationId("runtime")),
    metadata = Set(MetadataInfo("configuration-description", Set("this is a test conf"))),
    artifacts = Set(ArtifactRef(Hash("4335151"), Set(Attribute("configuration", Set("master"))), Some("filename-test.jar"))),
    attributes = Set(Attribute("attr3", Set("foo"))),
    requirements = Set(ConfiguredRequirement(Id("loo"), Set(ConfigurationId("compile"), ConfigurationId("master")),
      constraints = Set(Constraint("binary-version", Set("2.1"))))))

  val metadata = ConfiguredVariantsMetadata(
    id = Id("bar"),
    metadata = Set(MetadataInfo("authors", Set("fredrik", "arve")), MetadataInfo("license", Set.empty), MetadataInfo("homepage", Set("http://foo.com/bar"))),
    attributes = Set(Attribute("organization", Set("foo")), Attribute("version", Set("1.1.2")), Attribute("binary-version", Set("1.0", "1.1"))), //backwards compatible: because 1.1 and 1.0
    configurations = Set(
      testConf,
      compileConf,
      runtimeConf))
}

class ConfiguredVariantsMetadataTest extends FunSuite with MustMatchers {
  import ConfiguredVariantsMetadataTestData._

  test("Serialization of ConfiguredVariantsMetadata") {
    val writer = new StringWriter()
    metadata.toJson(writer)
    val jsonString = writer.getBuffer().toString()
    val reader = new StringReader(jsonString)
    val deserializedMetdata = ConfiguredVariantsMetadata.fromJson(reader)
    deserializedMetdata must be === metadata
  }

  test("Transformation of Variants") {

    val variantsMetadata = metadata.toVariants
    variantsMetadata.map { variant => variant.id.value } must be === Set("bar", "bar/config/runtime", "bar/config/compile", "bar/config/test")

    //FIXME: add test that tests:
    //(foo/bar/config/test [organization=(foo),version=(1.1.2),binary-version=(1.0,1.1),attr3=(foo)],Set(RepositoryMetadata(zoo.com,Set(git@git://github.com/zoo/loo.git),loo123commit,version = 2.1.1)))
    //(foo/bar/config/compile [organization=(foo),version=(1.1.2),binary-version=(1.0,1.1),attr1=(foo)],Set(RepositoryMetadata(loco.com,Set(git@git://github.com/loco/mooboo.git),mooboo456commit,version = 1.0.0)))
    //(foo/bar/config/runtime [organization=(foo),version=(1.1.2),binary-version=(1.0,1.1),attr2=(foo)],Set())
    //(foo/bar [organization=(foo),version=(1.1.2),binary-version=(1.0,1.1),configuration-hash=(4618fa38119766489ea263349107e20e8518bd0872e43ca7a5aba0e0aaaa4f07)],Set())
    println(variantsMetadata.mkString("\n"))

    pending
  }
}