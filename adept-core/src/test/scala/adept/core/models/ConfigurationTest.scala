package adept.core.models

import org.scalatest._
import adept.core.operations._

class ConfigurationTest extends FunSuite with MustMatchers {
  import adept.core.tests.TestData._

  test("configuration mapping") {
    Configuration.defaultConfigurationMapping("", "") must be === "*->*"
    Configuration.defaultConfigurationMapping("test", "") must be === "test->test"
    Configuration.defaultConfigurationMapping("runtime", "") must be === "runtime->runtime"
    Configuration.defaultConfigurationMapping("", "runtime->*;test->default") must be === "runtime->*;test->default"
    Configuration.defaultConfigurationMapping("test", "runtime->*;test->default") must be === "test->default"
    Configuration.defaultConfigurationMapping("runtime", "runtime->*;test->default") must be === "runtime->*"
    Configuration.defaultConfigurationMapping("", "*->default(compile)") must be === "*->default(compile)"
    Configuration.defaultConfigurationMapping("test", "*->default(compile)") must be === "test->default(compile)"
    Configuration.defaultConfigurationMapping("runtime", "*->default(compile)") must be === "runtime->default(compile)"
    Configuration.defaultConfigurationMapping("compile->runtime", "*->default(compile)") must be === "compile->runtime"
  }
  
  test("binding single configuration expressions") {
    import org.scalatest.EitherValues._
    val testConfs = configurations.filter(c => c.name == "test") 
    val compileConfs = configurations.filter(c => c.name == "compile") 
    ConfigurationResolver.resolve(testConfs, "goo->foo(*)", configurations) must be ('left)
    ConfigurationResolver.resolve(testConfs, "test->foo", configurations) must be ('left)
    ConfigurationResolver.resolve(testConfs, "test->test", configurations) must be ('left)
    ConfigurationResolver.resolve(testConfs, "test->compile", configurations).right.value.map(_.name) must be === Set("compile")
    ConfigurationResolver.resolve(testConfs, "*->default(compile)", configurations).right.value.map(_.name) must be === Set("default")
    ConfigurationResolver.resolve(testConfs, "*->foo(*)", configurations).right.value.map(_.name) must be === Set("default", "provided", "master", "compile", "sources", "runtime")
    ConfigurationResolver.resolve(testConfs, "*->default(compile),master(*)", configurations).right.value.map(_.name) must be === Set("default", "master")
    ConfigurationResolver.resolve(testConfs, "*->default(compile),master(*),foo(sources)", configurations).right.value.map(_.name) must be === Set("default", "master", "sources")
    ConfigurationResolver.resolve(compileConfs, "compile->compile(*),master(*)", configurations).right.value.map(_.name) must be === Set("compile", "master")
  }
  
  test("binding multiple configuration expressions") {
    import org.scalatest.EitherValues._
    val testConfs = configurations.filter(c => c.name == "test") 
    val compileConfs = configurations.filter(c => c.name == "compile") 
    ConfigurationResolver.resolve(testConfs, "default->master;compile->default", configurations)
    
    ConfigurationResolver.resolve(testConfs, "test->master;compile->default", configurations).right.value.map(_.name) must be === Set("master")
    ConfigurationResolver.resolve(testConfs, "test->master;test->default", configurations).right.value.map(_.name) must be === Set("master", "default")
    ConfigurationResolver.resolve(testConfs, "test->test;compile->default", configurations) must be ('left)
    ConfigurationResolver.resolve(testConfs, "default->master;compile->default", configurations) must be ('left)
    
    ConfigurationResolver.resolve(compileConfs, "compile->compile(*),master(*);runtime->runtime(*)", configurations).right.value.map(_.name) must be === Set("compile", "master")
    
  }
}