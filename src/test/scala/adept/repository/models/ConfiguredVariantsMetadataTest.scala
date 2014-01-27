package adept.repository.models

import org.scalatest.FunSuite
import org.scalatest.matchers.MustMatchers
import adept.models._
import adept.repository.models.configuration._
import java.io.StringWriter
import java.io.StringReader

class ConfiguredVariantsMetadataTest extends FunSuite with MustMatchers {

  test("Serialization of ConfiguredVariantsMetadata") {
    val metadata = ConfiguredVariantsMetadata(
      id = Id("foo/bar"),
      metadata = Set(MetadataInfo("authors", Set("fredrik", "arve")), MetadataInfo("license", Set.empty), MetadataInfo("homepage", Set("http://foo.com/bar"))),
      attributes = Set(Attribute("organization", Set("foo")), Attribute("version", Set("1.1.2")), Attribute("binary-version", Set("1.0", "1.1"))), //backwards compatible: because 1.1 and 1.0
      configurations = Set(
        Configuration(
          id = ConfigurationId("test"),
          extendsConfigurations = Set(ConfigurationId("compile"), ConfigurationId("runtime")),
          metadata = Set(MetadataInfo("something", Set("special"))),
          artifacts = Set(ArtifactRef(Hash("12345"), Set(Attribute("configuration", Set("master"))), Some("filename-1.jar"))),
          attributes = Set(Attribute("attr1", Set("foo"))),
          requirements = Set(ConfiguredRequirement(Id("loo"), Set(ConfigurationId("compile"), ConfigurationId("master")),
            constraints = Set(Constraint("binary-version", Set("2.1"))),
            commit = RepositoryMetadata("zoo.com", commit = Commit("123abed"), uris = Set("git@git://github.com/zoo/loo.git"), info = "version = 2.1.1"))))))

    val writer = new StringWriter()
    metadata.toJson(writer)
    val jsonString = writer.getBuffer().toString()
    val reader = new StringReader(jsonString)
    val deserializedMetdata = ConfiguredVariantsMetadata.fromJson(reader)
    deserializedMetdata must be === metadata
  }
}