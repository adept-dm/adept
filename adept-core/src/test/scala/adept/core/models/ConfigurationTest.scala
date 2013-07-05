package adept.core.models

import org.scalatest._
import adept.core.operations._

class ConfigurationTest extends FunSuite with MustMatchers {

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

}