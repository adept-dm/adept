package adept.ivy

import org.scalatest._
import org.scalatest.matchers.MustMatchers
import org.apache.ivy.plugins.resolver.URLResolver
import org.apache.ivy.core.module.id.ModuleRevisionId
import org.apache.ivy.core.IvyContext
import java.io.File
import adept.ext.conversions._
import adept.models.Id
import adept.models.Constraint
import adept.models.Attribute
import adept.ext.AttributeDefaults
import adept.resolution.Resolver
import org.eclipse.jgit.api.Git
import adept.repository.GitVariantsLoader

class IvyHelperTest extends FunSuite with MustMatchers {

  test("Ivy basic import tests") {
//    val ivy = IvyHelper.load()
//    val ivyHelper = new IvyHelper(ivy)
//
//    import EitherValues._
//    val results = ivyHelper.ivyImport("com.typesafe.akka", "akka-actor_2.10", "2.1.0").right.value
//
//    results must have size (3)
//    results.map(_.mrid.toString) must be === Set("org.scala-lang#scala-library;2.10.0", "com.typesafe#config;1.0.0", "com.typesafe.akka#akka-actor_2.10;2.1.0")
    pending //TODO: create a integration test plan for Ivy tests  and uncomment + fix test
  }

  test("Ivy advanced tests (uses other resolvers, more complicated resolution)") {

    //        val ivy = IvyHelper.load(Some("src/test/resources/ivysettings.xml"), ivyLogger = IvyHelper.debugIvyLogger)

    //    val ivyHelper = new IvyHelper(ivy)
    //    val results = ivyHelper.ivyImport("com.typesafe.play", "play-json_2.10", "2.2.1").right.value

    //    results.foreach { result =>
    //      println(result.variantsMetadata.toString)
    //    }
    //
    //    ivyHelper.insert(results, baseDir)
  }

  import adept.test.FileUtils._

  val semanticVersion = {
    //    val semanticVersionIds = Set(
    //        Id("akka-actor") -> "com.typesafe.akka", 
    //        Id("scala-library") -> "org.scala-lang", 
    //        Id("config") -> "com.typesafe",
    //      Id("akka-actor_2.10") -> "com.typesafe.akka",
    //      Id("play") -> "com.typesafe.play",
    //      Id("play-exceptions") -> "com.typesafe.play",
    //      Id("play-iteratees") -> "com.typesafe.play",
    //      Id("sbt-link") -> "com.typesafe.play",
    //      Id("play-datacommons") -> "com.typesafe.play",
    //      Id("play-functional") -> "com.typesafe.play",
    //      Id("play-json") -> "com.typesafe.play",
    //      Id("templates") -> "com.typesafe.play")
    val semanticVersionIds = Set(
      Id("akka-actor"), // -> "com.typesafe.akka",
      Id("scala-library"), // -> "org.scala-lang",
      Id("config"), // -> "com.typesafe",
      Id("akka-actor_2.10"), // -> "com.typesafe.akka",
      Id("play"), // -> "com.typesafe.play",
      Id("play-exceptions"), // -> "com.typesafe.play",
      Id("play-iteratees"), // -> "com.typesafe.play",
      Id("sbt-link"), // -> "com.typesafe.play",
      Id("play-datacommons"), // -> "com.typesafe.play",
      Id("play-functional"), // -> "com.typesafe.play",
      Id("play-json"), // -> "com.typesafe.play",
      Id("templates")) // -> "com.typesafe.play")

    new SemanticVersion(semanticVersionIds)
  }

  def convert(ivyResults: Either[_, Set[IvyImportResult]]): Set[IvyImportResult] = {
    import OptionValues._
    import EitherValues._
    val projectedResuluts = ivyResults.right.value
    val all = projectedResuluts.map(_.variantsMetadata)
    val scalaConverted = projectedResuluts.map(_.convertWith(ScalaBinaryVersion, all).value)
    scalaConverted.map(_.convertWith(semanticVersion, scalaConverted.map(_.variantsMetadata)).value)
  }

  test("End to end basic test") {
//    usingTmpDir { tmpDir =>
//      val baseDir = tmpDir
//      val ivy = IvyHelper.load(ivyLogger = IvyHelper.warnIvyLogger)
//
//      ivy.configure(new File("src/test/resources/typesafe-ivy-settings.xml"))
//      val ivyHelper = new IvyHelper(ivy)
//
//      //hack to adjust scala library, should be possible to do in a different way
//      val akka205WithAdjustedScalaLib = {
//        import AttributeDefaults._
//        val scalaLibBinaryVersion = Set("2.9.2")
//
//        convert(ivyHelper.ivyImport("com.typesafe.akka", "akka-actor", "2.0.5")).map { r =>
//          val configurations = r.variantsMetadata.configurations.map { c =>
//            val requirements = c.requirements.map { r =>
//              if (r.id == ScalaBinaryVersion.ScalaLibId) {
//                r.copy(constraints = r.constraints.filter(_.name != BinaryVersionAttribute) + Constraint(BinaryVersionAttribute, scalaLibBinaryVersion))
//              } else r
//            }
//            c.copy(requirements = requirements)
//          }
//          r.copy(variantsMetadata = r.variantsMetadata.copy(configurations = configurations))
//        }.map { r =>
//          val variantsMetadata = {
//            if (r.variantsMetadata.id == ScalaBinaryVersion.ScalaLibId) {
//              val attributes = r.variantsMetadata.attributes.filter(_.name != BinaryVersionAttribute) + Attribute(BinaryVersionAttribute, scalaLibBinaryVersion)
//              r.variantsMetadata.copy(attributes = attributes)
//            } else {
//              r.variantsMetadata
//            }
//          }
//
//          r.copy(variantsMetadata = variantsMetadata)
//        }
//      }
//
//      //      IvyHelper.insert(convert(ivyHelper.ivyImport("com.typesafe", "config", "0.3.1")), baseDir)
//      //      IvyHelper.insert(convert(ivyHelper.ivyImport("com.typesafe", "config", "1.0.0")), baseDir)
//      IvyHelper.insert(akka205WithAdjustedScalaLib, baseDir)
////      IvyHelper.insert(convert(ivyHelper.ivyImport("com.typesafe.akka", "akka-actor_2.10", "2.1.0")), baseDir)
//            IvyHelper.insert(convert(ivyHelper.ivyImport("com.typesafe.akka", "akka-actor_2.10", "2.2.0")), baseDir)
//            IvyHelper.insert(convert(ivyHelper.ivyImport("com.typesafe.akka", "akka-actor_2.10", "2.2.1")), baseDir)
//      //      IvyHelper.insert(convert(ivyHelper.ivyImport("com.typesafe.akka", "akka-actor_2.10", "2.2.2")), baseDir)
//            IvyHelper.insert(convert(ivyHelper.ivyImport("com.typesafe.play", "play_2.10", "2.2.0")), baseDir)
//            IvyHelper.insert(convert(ivyHelper.ivyImport("com.typesafe.play", "play_2.10", "2.2.1")), baseDir)
//
//    }
    pending //TODO: create a integration test plan for Ivy tests  and uncomment + fix test
  }
}