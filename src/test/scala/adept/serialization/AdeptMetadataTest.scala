package adept.serialization

import org.scalatest.FunSuite
import org.scalatest.Matchers
import adept.core.models.{ Dependency => _, _ }
import java.io.ByteArrayOutputStream
import java.io.StringWriter
import adept.configuration.ConfigurationId
import adept.repository.Commit
import java.io.StringReader

class AdeptMetadataTest extends FunSuite with Matchers {

  test("basic serialization") {

    val adeptMetadata = AdeptMetadata(
      id = Id("akka-actors"),
      metadata = Set(Metadata("license", Set("apache-2.0"))),
      attributes = Set(Attribute("organization", Set("com.typesafe.akka")), Attribute("version", Set("2.2.1"))),
      configurations = Set(
        Configuration(
          id = ConfigurationId("runtime"),
          `extends` = Set("compile"),
          metadata = Set(Metadata("configuration-description", Set("the jars you need in runtime"))),
          artifacts = Set(ArtifactRef(Hash("12314abedf"), Set(Attribute("configuration", Set("doc"))), Option("http://location/foo-javadoc.jar"))),
          attributes = Set(Attribute("configuration-hash", Set("132131"))),
          dependencies = Set(
            Dependency(Id("config"), configurations = Set("compile", "master"), constraints = Set(Constraint("binary-version", Set("1.0"))), repositories = Set(RepositoryMetadata(name = "", uris = Set("git@git://github.com/adepthub/com.typesafe"), commit = Commit("abadas"), info = "version 1.0.2")))))))

    val writer = new StringWriter()
    adeptMetadata.toJson(writer)
    val jsonString = writer.getBuffer().toString

    val jsonReader = new StringReader(jsonString)
    val readAdeptMetadata = AdeptMetadata.fromJson(jsonReader)

    //    println(jsonString)
    //    println(readAdeptMetadata)
    //    println("id",readAdeptMetadata.id == adeptMetadata.id)
    //    println("metadata",readAdeptMetadata.metadata == adeptMetadata.metadata)
    //    println("attributes",readAdeptMetadata.attributes == adeptMetadata.attributes)
    //    println("configurations",readAdeptMetadata.configurations == adeptMetadata.configurations)
    //    val configuration = adeptMetadata.configurations.head
    //    val readConfiguration = readAdeptMetadata.configurations.head
    //    println("configuration.id",configuration.id == readConfiguration.id)
    //    println("configuration.extends",configuration.`extends` == readConfiguration.`extends`)
    //    println("configuration.extends.head",configuration.`extends`.head == readConfiguration.`extends`.head, configuration.`extends`.head, readConfiguration.`extends`.head)
    //    println("configuration.metadata",configuration.metadata == readConfiguration.metadata)
    //    println("configuration.artifacts",configuration.artifacts == readConfiguration.artifacts)
    //    println("configuration.attributes",configuration.attributes == readConfiguration.attributes)
    //    println("configuration.dependencies",configuration.dependencies == readConfiguration.dependencies)
    // 
    readAdeptMetadata should be === adeptMetadata
  }
}