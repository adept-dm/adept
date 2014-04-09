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
import adept.ext.VersionRank
import adept.resolution.models.Id
import adept.repository.models.RepositoryName
import adept.repository.GitRepository
import org.apache.ivy.Ivy

class IvyIntegrationTest extends FunSuite with Matchers {
  import adept.test.FileUtils._
  import adept.test.IvyTestUtils

  val transitive = true
  val changing = false //This should always be true, except when debugging
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
      installScalaWithBinaryVersions(tmpDir, ivy, Set("2.10.0", "2.10.2"), changing = changing)
      IvyTestUtils.verify(tmpDir, ivy, ivyModule, changing = changing)
    }
  }

  def installScalaWithBinaryVersions(baseDir: File, ivy: Ivy, versions: Set[String], changing: Boolean)(implicit testDetails: TestDetails) = {
    val ivyModule = getDefaultAdeptModule

    versions.foreach { scalaVersion =>
      val scalaLibDep = new DefaultDependencyDescriptor(ivyModule,
        ModuleRevisionId.newInstance("org.scala-lang", "scala-compiler", scalaVersion), force, changing, transitive)
      scalaLibDep.addDependencyConfiguration("compile", "default(compile)")
      ivyModule.addDependency(scalaLibDep)
      IvyTestUtils.verify(baseDir, ivy, ivyModule, changing = changing)
    }

    val scalaIds = Set("", "/config/compile", "/config/default", "/config/javadoc", "/config/master", "/config/provided", "/config/runtime", "/config/sources", "/config/system").map { confString =>
      Id("org.scala-lang/scala-library" + confString)
    }
    val scalaRepoName = RepositoryName("org.scala-lang")
    val scalaRepo = new GitRepository(baseDir, scalaRepoName)
    val scalaCommit = scalaRepo.getHead
    var (addFiles, rmFiles) = Set.empty[File] -> Set.empty[File]
    scalaIds.foreach { scalaId =>
      val (currentAddFiles, currentRmFiles) = VersionRank.useSemanticVersionRanking(scalaId, scalaRepo, scalaCommit, includes = Set("2\\.11.\\d+".r, "2\\.10.*".r, "2\\.9.*".r), excludes = Set(".*".r), useVersionAsBinary = Set("2\\.8.*".r, "2\\.7.*".r, "2\\.6.*".r, "2\\.5.*".r, "2\\.4.*".r, "2\\.3.*".r))
      addFiles ++= currentAddFiles
      rmFiles ++= currentRmFiles
    }
    scalaRepo.add(addFiles)
    scalaRepo.rm(rmFiles)
    scalaRepo.commit("Versioned Scala")
  }

  test("Ivy end-to-end: scala-compiler 2.10.2 (module with optional deps)") {
    implicit val testDetails = TestDetails("End-to-end (scala-compiler 2.10.2 optional deps)")
    usingTmpDir { tmpDir =>
      val ivy = IvyTestUtils.ivy
      installScalaWithBinaryVersions(tmpDir, ivy, Set("2.10.2"), changing)
    }
  }

  test("Ivy end-to-end: scala-library 2.10.3 (module with no deps)") {
    implicit val testDetails = TestDetails("End-to-end (scala-library 2.10.3 no deps)")
    usingTmpDir { tmpDir =>
      val ivy = IvyTestUtils.ivy
      installScalaWithBinaryVersions(tmpDir, ivy, Set("2.10.3"), changing)
    }
  }

  test("Ivy end-to-end: adepts own (many different)") {
    pending
  }

  test("Ivy end-to-end: scala & akka & versioning") {
    implicit val testDetails = TestDetails("End-to-end (scala & akka & versioning)")
    usingTmpDir { tmpDir =>
      val ivy = IvyTestUtils.ivy

      installScalaWithBinaryVersions(tmpDir, ivy, Set("2.9.3", "2.10.1", "2.10.2"), changing)

      {
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

        IvyTestUtils.verify(tmpDir, ivy, ivyModule, changing)
      }
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