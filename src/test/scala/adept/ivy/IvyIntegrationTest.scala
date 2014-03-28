package adept.ivy

import org.scalatest.FunSuite
import org.scalatest.Matchers
import adept.test.TestDetails
import java.io.File
import org.apache.ivy.core.module.descriptor.DefaultModuleDescriptor
import org.apache.ivy.core.module.descriptor.DefaultDependencyDescriptor
import org.apache.ivy.core.module.id.ModuleRevisionId
import org.apache.ivy.core.module.descriptor.DefaultExcludeRule
import org.apache.ivy.core.module.descriptor.{ Configuration => IvyConfiguration }
import org.apache.ivy.core.module.id.ArtifactId
import org.apache.ivy.core.module.id.ModuleId
import org.apache.ivy.plugins.matcher.ExactPatternMatcher

class IvyIntegrationTest extends FunSuite with Matchers {
  import adept.test.FileUtils._
  import adept.test.IvyTestUtils

  val transitive = true
  val changing = true
  val force = true

  def getDefaultAdeptModule = {
    val ivyModule = DefaultModuleDescriptor.newBasicInstance(ModuleRevisionId.newInstance("com.adepthub", "test", "1.0"), new java.util.Date(1395315115209L))
    ivyModule.addConfiguration(new IvyConfiguration("default", IvyConfiguration.Visibility.PUBLIC, "", Array("master", "runtime"), true, ""))
    ivyModule.addConfiguration(new IvyConfiguration("master", IvyConfiguration.Visibility.PUBLIC, "", Array.empty, true, ""))
    ivyModule.addConfiguration(new IvyConfiguration("runtime", IvyConfiguration.Visibility.PUBLIC, "", Array("compile"), true, ""))
    ivyModule.addConfiguration(new IvyConfiguration("compile", IvyConfiguration.Visibility.PUBLIC, "", Array.empty, true, ""))
    ivyModule.addConfiguration(new IvyConfiguration("test", IvyConfiguration.Visibility.PRIVATE, "", Array("runtime"), true, ""))
    ivyModule
  }

  test("Ivy end-to-end: akka-remote & scalatest (w/exclude rules)") {
    implicit val testDetails = TestDetails("End-to-end (akka-remote & scalatest & excludes)")
    usingTmpDir { tmpDir =>
      val ivy = IvyTestUtils.ivy
      ivy.configure(new File("src/test/resources/typesafe-ivy-settings.xml"))

      val ivyModule = getDefaultAdeptModule
      val akkaDep = new DefaultDependencyDescriptor(ivyModule,
        ModuleRevisionId.newInstance("com.typesafe.akka", "akka-remote_2.10", "2.2.1"), force, changing, transitive)
      akkaDep.addExcludeRule("compile", new DefaultExcludeRule(new ArtifactId(new ModuleId("com.google.protobuf", "protobuf-java"), "*", "*", "*"), ExactPatternMatcher.INSTANCE, new java.util.HashMap()))
      akkaDep.addDependencyConfiguration("compile", "default(compile)")
      ivyModule.addDependency(akkaDep)
      val scalaTestDep = new DefaultDependencyDescriptor(ivyModule,
        ModuleRevisionId.newInstance("org.scalatest", "scalatest_2.10", "1.9.1"), force, changing, transitive)
      scalaTestDep.addDependencyConfiguration("test", "default(compile)")
      ivyModule.addDependency(scalaTestDep)

      IvyTestUtils.verify(tmpDir, ivy, ivyModule)
    }
  }

  test("Ivy end-to-end: scala-compiler 2.10.2 (module with optional deps)") {
    implicit val testDetails = TestDetails("End-to-end (scala-compiler 2.10.2 optional deps)")
    usingTmpDir { tmpDir =>
      val ivy = IvyTestUtils.ivy
      val ivyModule = getDefaultAdeptModule

      val scalaLibDep = new DefaultDependencyDescriptor(ivyModule,
        ModuleRevisionId.newInstance("org.scala-lang", "scala-compiler", "2.10.2"), force, changing, transitive)
      scalaLibDep.addDependencyConfiguration("compile", "default(compile)")
      ivyModule.addDependency(scalaLibDep)

      IvyTestUtils.verify(tmpDir, ivy, ivyModule)
    }
  }

  test("Ivy end-to-end: scala-library 2.10.3 (module with no deps)") {
    implicit val testDetails = TestDetails("End-to-end (scala-library 2.10.3 no deps)")
    usingTmpDir { tmpDir =>
      val ivy = IvyTestUtils.ivy
      val ivyModule = getDefaultAdeptModule

      val scalaLibDep = new DefaultDependencyDescriptor(ivyModule,
        ModuleRevisionId.newInstance("org.scala-lang", "scala-library", "2.10.3"), force, changing, transitive)
      scalaLibDep.addDependencyConfiguration("compile", "default(compile)")
      ivyModule.addDependency(scalaLibDep)
      
      IvyTestUtils.verify(tmpDir, ivy, ivyModule)
    }
  }

  //  test("Ivy end-to-end: sbt plugin") {
  //    implicit val testDetails = TestDetails("End-to-end (play sbt plugin)")
  //    usingTmpDir { tmpDir =>
  //      val  ivy = IvyUtils.load(ivyLogger = IvyConstants.warnIvyLogger)
  //
  //      ivy.configure(new File("src/test/resources/sbt-plugin-ivy-settings.xml"))
  //      
  //      val ivyConverter = new IvyAdeptConverter(ivy)
  //
  //      val transitive = true
  //      val changing = true
  //      val force = true
  //      val ivyModule = DefaultModuleDescriptor.newBasicInstance(ModuleRevisionId.newInstance("com.adepthub", "test", "1.0"), new java.util.Date(1395315115209L))
  //      ivyModule.addConfiguration(new IvyConfiguration("default", IvyConfiguration.Visibility.PUBLIC, "", Array("master", "runtime"), true, ""))
  //      ivyModule.addConfiguration(new IvyConfiguration("master", IvyConfiguration.Visibility.PUBLIC, "", Array.empty, true, ""))
  //      ivyModule.addConfiguration(new IvyConfiguration("runtime", IvyConfiguration.Visibility.PUBLIC, "", Array("compile"), true, ""))
  //      ivyModule.addConfiguration(new IvyConfiguration("compile", IvyConfiguration.Visibility.PUBLIC, "", Array.empty, true, ""))
  //      ivyModule.addConfiguration(new IvyConfiguration("test", IvyConfiguration.Visibility.PRIVATE, "", Array("runtime"), true, ""))
  //
  //      val playSbtDep = new DefaultDependencyDescriptor(ivyModule,
  //        ModuleRevisionId.newInstance("com.typesafe.play", "sbt-plugin", "2.2.2"), force, changing, transitive)
  //      playSbtDep.addDependencyConfiguration("runtime", "default(runtime)")
  //      ivyModule.addDependency(playSbtDep)
  //
  //      val results = benchmark(IvyImport, ivyModule) {
  //        ivyConverter.loadAsIvyImportResults(ivyModule, progress).failOnLeft
  //      }
  //      val resolutionResults = benchmark(Inserted, results) {
  //        IvyImportResultInserter.insertAsResolutionResults(tmpDir, results, progress)
  //      }
  //
  //      val requirements = benchmark(Converted, ivyModule && results) {
  //        IvyRequirements.convertIvyAsRequirements(ivyModule, results)
  //      }
  //
  //      val loader = benchmark(Loaded, resolutionResults) {
  //        new GitLoader(tmpDir, resolutionResults, progress, cacheManager)
  //      }
  //
  //      for (confName <- requirements.keys) {
  //        val result = benchmark(Resolved, requirements(confName) && loader) {
  //          resolve(requirements(confName), loader)
  //        }
  //        println(result)
  //      }
  //    }
  //  }
}