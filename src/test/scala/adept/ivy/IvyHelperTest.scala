package adept.ivy

import org.scalatest._
import org.scalatest.matchers.MustMatchers
import org.apache.ivy.plugins.resolver.URLResolver
import org.apache.ivy.core.module.id.ModuleRevisionId
import org.apache.ivy.core.IvyContext
import java.io.File
import adept.ext.conversions._
import adept.models.Id

class IvyHelperTest extends FunSuite with MustMatchers {

  test("Ivy basic import tests") {
    val ivy = IvyHelper.load()
    val ivyHelper = new IvyHelper(ivy)

    import EitherValues._
    val results = ivyHelper.ivyImport("com.typesafe.akka", "akka-actor_2.10", "2.1.0").right.value

    results must have size (3)
    results.map(_.mrid.toString) must be === Set("org.scala-lang#scala-library;2.10.0", "com.typesafe#config;1.0.0", "com.typesafe.akka#akka-actor_2.10;2.1.0")
  }

  test("Ivy advanced tests (uses other resolvers, more complicated resolution)") {

    //    val ivy = IvyHelper.load(Some("src/test/resources/ivysettings.xml"), ivyLogger = IvyHelper.debugIvyLogger)

    //    val ivyHelper = new IvyHelper(ivy)
    //    val results = ivyHelper.ivyImport("com.typesafe.play", "play-json_2.10", "2.2.1").right.value

    //    results.foreach { result =>
    //      println(result.variantsMetadata.toString)
    //    }
    //
    //    ivyHelper.insert(results, baseDir)
  }

  test("Ivy insert test") {
    import EitherValues._
    val ivy = IvyHelper.load()
    val ivyHelper = new IvyHelper(ivy)
    //    val results = ivyHelper.ivyImport("net.sf.ehcache", "ehcache-core", "2.6.6").right.value

  }

  import adept.test.FileUtils._
  test("End to end basic test") {
    usingTmpDir { tmpDir =>
      val ivy = IvyHelper.load()
      val ivyHelper = new IvyHelper(ivy)
      val semanticVersion = {
          val semanticVersionIds = Set(Id("akka-actor"), Id("scala-library"), Id("config"))
          new SemanticVersion(semanticVersionIds)
      }
      def convert(ivyResults: Either[_, Set[IvyImportResult]]): Set[IvyImportResult] = {
        import OptionValues._
        import EitherValues._
        ivyResults.right.value.map(_.convertWith(ScalaBinaryVersion).value.convertWith(semanticVersion).value)
      }
      IvyHelper.insert(convert(ivyHelper.ivyImport("com.typesafe.akka", "akka-actor", "2.0.5")), tmpDir)
      println(convert(ivyHelper.ivyImport("com.typesafe.akka", "akka-actor_2.10", "2.1.0")).mkString("\n"))


      //      IvyHelper.insert(convert(ivyHelper.ivyImport("com.typesafe.akka", "akka-actor_2.10", "2.1.0")), tmpDir)
      //      IvyHelper.insert(convert(ivyHelper.ivyImport("com.typesafe.akka", "akka-actor_2.10", "2.2.0")), tmpDir)
      //      IvyHelper.insert(convert(ivyHelper.ivyImport("com.typesafe.akka", "akka-actor_2.10", "2.2.1")), tmpDir)
    }

  }
}