package adept.core.operations


import org.scalatest._
import adept.core.operations._

class ConflictResolverTest extends FunSuite with MustMatchers {
  import adept.core.tests.TestData._

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
    //TODO: ConfigurationResolver.resolve(testConfs, "default->master;compile->default", configurations)
    
    ConfigurationResolver.resolve(testConfs, "test->master;compile->default", configurations).right.value.map(_.name) must be === Set("master")
    ConfigurationResolver.resolve(testConfs, "test->master;test->default", configurations).right.value.map(_.name) must be === Set("master", "default")
    ConfigurationResolver.resolve(testConfs, "test->test;compile->default", configurations) must be ('left)
    ConfigurationResolver.resolve(testConfs, "default->master;compile->default", configurations) must be ('left)
    
    ConfigurationResolver.resolve(compileConfs, "compile->compile(*),master(*);runtime->runtime(*)", configurations).right.value.map(_.name) must be === Set("compile", "master")
    
  }
  
  test("binding multiple configuration with the same name to expressions") {
    /* TODO: should be somethinglike this, but I am not sure if this really tests that well
    import org.scalatest.EitherValues._
    val multiConfs = configurations + defaultConf.copy(description = Some("foo"))
    val testConfs = configurations.filter(c => c.name == "test") 
    val compileConfs = configurations.filter(c => c.name == "compile") 
    
    ConfigurationResolver.resolve(testConfs, "test->master;compile->default", multiConfs).right.value.map(_.name) must be === Set("master")
    ConfigurationResolver.resolve(testConfs, "test->master;test->default", multiConfs).right.value.map(_.name) must be === Set("master", "default")
    ConfigurationResolver.resolve(testConfs, "test->test;compile->default", multiConfs) must be ('left)
    ConfigurationResolver.resolve(testConfs, "default->master;compile->default", multiConfs) must be ('left)
    
    ConfigurationResolver.resolve(compileConfs, "compile->compile(*),master(*);runtime->runtime(*)", multiConfs).right.value.map(_.name) must be === Set("compile", "master")
    */
    pending
  }
}