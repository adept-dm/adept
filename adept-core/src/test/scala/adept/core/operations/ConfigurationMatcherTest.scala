package adept.core.operations

import adept.core.models._
import org.scalatest._
import adept.core.operations._

class ConfigurationMatcherTest extends FunSuite with MustMatchers {
  import adept.core.tests.TestData._
  import collection.{Set => _, _}
    
  test("find extended") {
    ConfigurationResolver.extendedConfs(configurations, testConf).map(_.name) must be === Set("compile", "runtime")
  }
  
  test("artifact works with basic configurations") {
    val module = adept10
    val compileConfs = {
      val c = ConfigurationResolver.resolve(module.configurations, "compile")
      c must be ('right)
      c.right.get
    }
    
    ConfigurationMatcher.matchingArtifacts(module.artifacts, compileConfs.toSet)._1.map(_.hash.value).toList.sorted must be === List("artihash2") 

    ConfigurationResolver.resolve(module.configurations, "strangeconf") must be ('left)
  }
  
  test("artifact works with wildcard configurations") {
    val module = adept10
    val allConfs = {
      val c = ConfigurationResolver.resolve(module.configurations, "*")
      c must be ('right)
      c.right.get
    }.toSet
    ConfigurationMatcher.matchingArtifacts(module.artifacts, allConfs)._1.map(_.hash) must have size(2)
  }
  
  test("artifact works with extended configurations") {
    val artifacts = Set(Artifact(Hash("artihash2"), "jar", Set("master"), Set("http://url.no/hash2.jar")))
    val defaultConfs = {
      val c = ConfigurationResolver.resolve(configurations, "default")
      c must be ('right)
      c.right.get
    }.toSet
    ConfigurationMatcher.matchingArtifacts(artifacts, defaultConfs)._1.map(_.hash) must have size(1)
  }
  
  test("dependency resolution with multiple configurations"){
    val confs = configurations
    val (moduleConfs, evicted, missing) = ConfigurationMatcher.matchingModules(adept10.coordinates, adept10.dependencies, Set.empty, confs, Configuration.defaultConfigurationMapping(_), findModule)
    evicted.map(_.reason) must have size(1) //neverinclude
    moduleConfs.map{case (m,t, e, c) => m.coordinates -> c.map(_.name) } must be === Set(testlib47.coordinates -> Set("default"), extralib30.coordinates -> Set("master"), extralib10.coordinates -> Set("master"), commonlib20.coordinates -> Set("default"))
  }
  
  test("dependency resolution with multiple (including similary named) configurations"){
    val confs = configurations + defaultConf.copy(description = Some("foo"))
    val (moduleConfs, evicted, missing) = ConfigurationMatcher.matchingModules(adept10.coordinates, adept10.dependencies, Set.empty, confs, Configuration.defaultConfigurationMapping(_), findModule)
    evicted.map(_.reason) must have size(1) //neverinclude
    moduleConfs.map{case (m,t, e, c) => m.coordinates -> c.map(_.name) } must be === Set(testlib47.coordinates -> Set("default"), extralib30.coordinates -> Set("master"), extralib10.coordinates -> Set("master"), commonlib20.coordinates -> Set("default"))
  }
  
  test("dependency resolution with compile configuration"){
    val confs = ConfigurationResolver.resolve(configurations, "compile").right.get.toSet
    val (moduleConfs, evicted, missing) = ConfigurationMatcher.matchingModules(adept10.coordinates, adept10.dependencies, Set.empty, confs, Configuration.defaultConfigurationMapping(_), findModule)
    evicted.map(_.reason) must have size(3)
    moduleConfs.map{case (m,t, e, c) => m.coordinates -> c.map(_.name) }.toSet must be === Set(commonlib20.coordinates -> Set("default"))
  }
  
  test("dependency resolution with exclusions configuration"){
    pending
  }
}