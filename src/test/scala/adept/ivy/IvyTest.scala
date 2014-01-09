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
import adept.configuration._

class IvyTest extends FunSuite with MustMatchers {

  test("basic ivy test") {
    import EitherValues._
    val ivy = IvyHelper.load().right.value
    val ivyHelper = new IvyHelper(ivy)
    val org = "com.typesafe.akka"
    val name = "akka-actor_2.10"
    val version = "2.2.1"
    val importResults = (ivyHelper.ivyImport(org, name, version)).right.value

    val variants = importResults.flatMap(_.variants)
    val resolver = new Resolver(new DefinedVariants(variants.toSeq))
    val result = resolver.resolve(ConfiguredDependency(Id(org + "/" + name), Set(ConfigurationId("compile"), ConfigurationId("master")), Set(Constraint("version", Set(version)))).toDependencies)
    println(result)
    result.state.isResolved must be === true
  }

  //TODO: split this test in 2: one for Ivy and one for conflict resolution?
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

    val variants = importResults.flatMap(_.variants)

    val loaderEngine = new DefinedVariants(variants.toSeq)
    val resolver = new Resolver(loaderEngine)
    val dependencies = inputCoords.map {
      case (org, name, version) =>
        Dependency(Id(org + "/" + name), Set(Constraint("version", Set(version))))
    }.toSet
    val failedResult = resolver.resolve(dependencies)
    unresolved(failedResult)

    val (result, newVariants) = VersionConflictResolver.resolveHighestConflicts(failedResult, dependencies, loaderEngine).right.value
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

  private def ivyImport(ivyHelper: IvyHelper, inputCoords: Seq[(String, String, String)]): Set[Variant] = {
    import EitherValues._
    import IvyHelper._

    println("starting ivy import...")
    val importResults = inputCoords.flatMap {
      case (org, name, version) =>
        (ivyHelper.ivyImport(org, name, version)).right.value
    }
    println("ivy import completed")

    val variants = importResults.flatMap(_.variants)
    variants.toSet
  }

  private def conflictResolution(variants: Set[Variant], inputCoords: Seq[(String, String, String)]): (Set[Variant], State) = {
    import EitherValues._
    import IvyHelper._

    val dependencies = inputCoords.map {
      case (org, name, version) =>
        Dependency(Id(org + "/" + name), Set(Constraint("version", Set(version))))
    }.toSet

    val loaderEngine = new DefinedVariants(variants.toSeq)
    val resolver = new Resolver(loaderEngine)
    val failedResult = resolver.resolve(dependencies)

    val (result, newVariants) = VersionConflictResolver.resolveHighestConflicts(failedResult, dependencies, loaderEngine).right.value

    println(result)
    val state = resolved(result)
    (variants ++ newVariants) -> state
  }

  //TODO: split this test in 2: one for Ivy and one for conflict resolution?
  test("triple ivy-like resolution") {
    import EitherValues._
    import IvyHelper._

    val ivy = IvyHelper.load().right.value
    val ivyHelper = new IvyHelper(ivy, changing = false) // TODO: changing should not be false

    val inputCoords1 = Seq(
      ("joda-time", "joda-time", "2.3"),
      ("com.fasterxml.jackson.core", "jackson-databind", "2.2.3"),
      ("org.slf4j", "slf4j-api", "1.7.5"),
      ("org.apache.httpcomponents", "httpclient", "4.3.1"),
      ("commons-io", "commons-io", "2.4"),
      ("commons-codec", "commons-codec", "1.8"),
      ("com.amazonaws", "aws-java-sdk", "1.5.6"),
      ("com.typesafe.akka", "akka-actor_2.10", "2.2.1"))

    val inputCoords2 = Seq(
      ("joda-time", "joda-time", "2.3"),
      ("com.fasterxml.jackson.core", "jackson-databind", "2.2.3"),
      ("org.slf4j", "slf4j-api", "1.7.5"),
      ("org.apache.httpcomponents", "httpclient", "4.3.1"),
      ("commons-io", "commons-io", "2.4"),
      ("commons-codec", "commons-codec", "1.8"),
      ("com.amazonaws", "aws-java-sdk", "1.5.4"),
      ("com.typesafe.akka", "akka-actor_2.10", "2.2.1"))

    val importedVariants1 = ivyImport(ivyHelper, inputCoords1)
    val (variants1, state1) = conflictResolution(importedVariants1, inputCoords1)
    println("+++++")
    println(variants1.map(_.fullString).mkString("\n"))
    println("+++++")
    val importedVariants2 = ivyImport(ivyHelper, inputCoords2)
    val (variants2, state2) = conflictResolution(variants1 ++ importedVariants1 ++ importedVariants2, inputCoords2)
    println("+++++")
    println(variants2.diff(variants1).map(_.fullString).mkString("\n"))
    println("+++++")
    val (variants3, state3) = conflictResolution(variants1 ++ importedVariants1 ++ variants2 ++ importedVariants2, inputCoords1)
    println("+++++")
    println(variants3.diff((variants2 ++ variants1)).mkString("\n"))
    println("+++++")

    //check all overrides:
    state1.resolvedVariants(Id("org.apache.httpcomponents/httpclient")).attribute("version").values must be === Set("4.3.1")
    state1.resolvedVariants(Id("org.apache.httpcomponents/httpcore")).attribute("version").values must be === Set("4.3")
    state1.resolvedVariants(Id("commons-codec/commons-codec")).attribute("version").values must be === Set("1.8")
    state1.resolvedVariants(Id("commons-logging/commons-logging")).attribute("version").values must be === Set("1.1.3")

    //just an addition random check: 
    state1.resolvedVariants(Id("org.codehaus.jackson/jackson-core-asl")).attribute("version").values must be === Set("1.8.9")

  }

}