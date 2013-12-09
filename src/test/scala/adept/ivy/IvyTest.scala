package adept.ivy

import org.scalatest._
import org.scalatest.matchers.MustMatchers
import adept.test.TestDSL._
import adept.test.TestHelpers._
import adept.core.models._
import adept.core.resolution.Resolver
import adept.ext.DefinedVariants
import org.apache.ivy.core.module.id.ModuleRevisionId
import adept.ext._
import org.apache.ivy.core.search.OrganisationEntry

class IvyTest extends FunSuite with MustMatchers {

  test("basic ivy test") {
    import EitherValues._
    val ivy = IvyHelper.load().right.value
    val ivyHelper = new IvyHelper(ivy)
    val org = "com.typesafe.akka"
    val name = "akka-actor_2.10"
    val version = "2.2.1"
    val importResults = (ivyHelper.ivyImport(org, name, version)).right.value

    val variants = importResults.map(_.variant)

    val resolver = new Resolver(new DefinedVariants(variants.toSeq))
    val result = resolver.resolve(Set(Dependency(Id(org + "/" + name), Set(Constraint("version", Set(version))))))
    result.state.isResolved must be === true
  }

  //TODO: split this test in 2: one for Ivy and one for resolution?
  test("ivy-like resolution") {
    import EitherValues._
    import IvyHelper._

    val ivy = IvyHelper.load().right.value
    val ivyHelper = new IvyHelper(ivy)
    
    val inputCoords = Seq(
      ("joda-time", "joda-time", "2.3"),
      ("com.fasterxml.jackson.core", "jackson-databind", "2.2.3"),
      ("org.slf4j", "slf4j-api", "1.7.5"),
      ("org.apache.httpcomponents", "httpclient", "4.3.1"),
      ("commons-io", "commons-io", "2.4"),
      ("commons-codec", "commons-codec", "1.8"),
      ("com.amazonaws", "aws-java-sdk", "1.5.5"),
      ("com.typesafe.akka", "akka-actor_2.10", "2.2.1"))

    println("starting ivy import...")
    val importResults = inputCoords.flatMap {
      case (org, name, version) =>
        (ivyHelper.ivyImport(org, name, version)).right.value
    }
    println("ivy import completed")
    importResults must have size 22

    
    val variants = importResults.map(_.variant)
    
    val loaderEngine = new DefinedVariants(variants.toSeq)
    val resolver = new Resolver(loaderEngine)
    val dependencies = inputCoords.map{ case (org, name, version) => 
      Dependency(Id(org + "/" + name), Set(Constraint("version", Set(version))))
    }.toSet
    val failedResult = resolver.resolve(dependencies)
    unresolved(failedResult)
    
    val (result, newVariants) = VersionConflictResolver.resolveHighestConflicts(failedResult, dependencies, loaderEngine)
    newVariants must have size 5
    
    val state = resolved(result)
    //check all overrides:
    state.resolvedVariants(Id("org.apache.httpcomponents/httpclient")).attribute("version").values must be === Set("4.3.1")
    state.resolvedVariants(Id("org.apache.httpcomponents/httpcore")).attribute("version").values must be === Set("4.3")
    state.resolvedVariants(Id("commons-codec/commons-codec")).attribute("version").values must be === Set("1.8")
    state.resolvedVariants(Id("commons-logging/commons-logging")).attribute("version").values must be === Set("1.1.3")
    
    //just an addition random check: 
    state.resolvedVariants(Id("org.codehaus.jackson/jackson-core-asl")).attribute("version").values must be === Set("1.8.9")
    
    
    
  }

}