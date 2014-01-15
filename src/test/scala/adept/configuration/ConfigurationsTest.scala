package adept.configuration

import org.scalatest.FunSuite
import org.scalatest.matchers.MustMatchers
import adept.core.models._
import adept.core.resolution.VariantsLoaderLogic
import adept.core.resolution.Resolver
import adept.ext.DefinedVariants
import adept.configuration._

class ConfigurationsTest extends FunSuite with MustMatchers {

  test("ivy like configurations") {

    val variantsA = VariantBuilder.create(Id("A"),
      attributes = Set(Attribute("version", Set("1.0"))),
      //COMPILE:
      artifacts = Set.empty,
      dependencies = Set(ConfiguredDependency(new Id("B"), Set(ConfigurationId("compile"), ConfigurationId("master")), Set.empty)),
      configuration = ConfigurationId("compile"),
      description = "this is the default scope, used if none is specified. Compile dependencies are available in all classpaths.")
      .withConfiguration( //MASTER:
        artifacts = Set(ArtifactRef(Hash("aeb123"), Set(Attribute("configuration", Set("default"))), None)),
        dependencies = Set.empty,
        ConfigurationId("master"), Set.empty, "contains only the artifact published by this module itself, with no transitive dependencies")
      .withConfiguration( //RUNTIME:
        artifacts = Set.empty,
        dependencies = Set.empty,
        ConfigurationId("runtime"), Set(ConfigurationId("compile")), "this scope indicates that the dependency is not required for compilation, but is for execution. It is in the runtime and test classpaths, but not the compile classpath.")
      .withConfiguration( //TEST:
          artifacts = Set.empty,
        dependencies = Set(ConfiguredDependency(new Id("B"), Set(ConfigurationId("runtime")), Set.empty)),
        ConfigurationId("test"), Set(ConfigurationId("runtime")), "this scope indicates that the dependency is not required for normal use of the application, and is only available for the test compilation and execution phases")
      .build()

    val variantsB = VariantBuilder.create(Id("B"),
      attributes = Set(Attribute("version", Set("1.0"))),
      //COMPILE:
      artifacts = Set.empty,
      dependencies = Set.empty,
      configuration = ConfigurationId("compile"),
      description = "this is the default scope, used if none is specified. Compile dependencies are available in all classpaths.")
      .withConfiguration( //MASTER:
        artifacts = Set(ArtifactRef(Hash("bcd567"), Set(Attribute("configuration", Set("default"))), None)),
        dependencies = Set.empty,
        ConfigurationId("master"), Set.empty, "contains only the artifact published by this module itself, with no transitive dependencies")
      .withConfiguration( //RUNTIME:
        artifacts = Set.empty,
        dependencies = Set.empty,
        ConfigurationId("runtime"), Set(ConfigurationId("compile")), "this scope indicates that the dependency is not required for compilation, but is for execution. It is in the runtime and test classpaths, but not the compile classpath.")
      .withConfiguration( //TEST:
          artifacts = Set.empty,
        dependencies = Set.empty,
        ConfigurationId("test"), Set(ConfigurationId("runtime")), "this scope indicates that the dependency is not required for normal use of the application, and is only available for the test compilation and execution phases")
      .build()

    val resolver = new Resolver(new DefinedVariants((variantsA ++ variantsB).toSeq))

    println(resolver.resolve(Set(ConfiguredDependency(Id("A"), Set(ConfigurationId("compile"), ConfigurationId("master")), Set.empty)).flatMap(_.toDependencies)))

    //
    //resolver.resolve(Set(Dependency(Id(""), Set(Constraint("", Set(""))), ConfigurationId("compile"))))

  }
}