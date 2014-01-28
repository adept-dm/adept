package adept.ivy

import org.scalatest._
import org.scalatest.matchers.MustMatchers
import org.apache.ivy.plugins.resolver.URLResolver
import org.apache.ivy.core.module.id.ModuleRevisionId
import org.apache.ivy.core.IvyContext
import java.io.File

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
    val ivy = IvyHelper.load(Some("src/test/resources/ivysettings.xml"), ivyLogger = IvyHelper.debugIvyLogger)
    
    
        ivy.resolve(ModuleRevisionId.newInstance("com.typesafe.play", "play-json_2.10", "2.2.1"), IvyHelper.resolveOptions(), true)
    
    
    //val ivyHelper = new IvyHelper(ivy)

    import EitherValues._

    //val results = ivyHelper.ivyImport("com.typesafe.play", "play-json_2.10", "2.2.1").right.value
    //results.foreach { result =>
      //println(result.variantsMetadata.toString)
    //}

    //ivyHelper.insert(results, baseDir)
  }
}