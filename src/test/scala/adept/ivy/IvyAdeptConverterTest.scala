package adept.ivy

import org.scalatest.FunSuite
import org.scalatest.Matchers
import adept.ext.Version
import adept.repository.models.VariantHash
import adept.repository.models.RepositoryName
import adept.repository.models.ResolutionResult
import adept.repository.models.Commit
import adept.repository.models.VariantHash
import adept.repository.serialization.ResolutionResultsMetadata
import java.io.File
import adept.repository.GitRepository
import adept.repository.GitLoader
import adept.repository.serialization.VariantMetadata
import adept.utils.Hasher
import net.sf.ehcache.CacheManager
import org.eclipse.jgit.lib.TextProgressMonitor
import adept.lockfile.Lockfile
import adept.artifact.models.Artifact
import adept.artifact.models.ArtifactHash
import adept.resolution.models.Variant
import adept.resolution.models.Constraint
import adept.resolution.models.Id
import adept.resolution.models.Requirement
import adept.ext.VersionScanner
import org.apache.ivy.core.module.descriptor.ModuleDescriptor
import org.apache.ivy.core.module.descriptor.{ Configuration => IvyConfiguration }
import org.apache.ivy.core.module.descriptor.DefaultModuleDescriptor
import org.apache.ivy.core.module.id.ModuleRevisionId
import org.apache.ivy.core.module.descriptor.ExcludeRule
import org.apache.ivy.core.module.descriptor.DefaultExcludeRule
import org.apache.ivy.core.module.descriptor.DefaultArtifact
import org.apache.ivy.core.module.id.ArtifactId
import org.apache.ivy.core.module.id.ModuleId
import org.apache.ivy.plugins.matcher.ExactPatternMatcher
import org.apache.ivy.core.module.descriptor.DefaultDependencyDescriptor
import adept.resolution.resolver.models.ResolvedResult
import adept.resolution.models.Attribute
import adept.test.TestDetails

class IvyAdeptConverterTest extends FunSuite with Matchers {
  import adept.test.ResolverUtils._
  import adept.test.LoaderUtils._
  import adept.test.FileUtils.usingTmpDir
  import adept.test.BenchmarkUtils._ //convert to benchmark hashes
  import adept.test.OutputUtils._
  import adept.test.EitherUtils._
  import adept.test.ArtifactUtils._

  import IvyConstants._
  import IvyUtils.withConfiguration
  import adept.ext.AttributeDefaults._

  def ivy = this.synchronized { //avoid parallel test messing up Ivy imports
    IvyUtils.load()
  }

  val akkaTransitiveIds: Set[Id] = Set(
    "com.typesafe.akka/akka-actor_2.10",
    "org.scala-lang/scala-library",
    "com.typesafe/config")
  val akkaTransitiveIdsExpectedIds =
    (for {
      id <- akkaTransitiveIds
      confName <- Set("optional", "runtime", "provided", "javadoc", "system", "default", "sources", "compile")
    } yield {
      withConfiguration(id, confName)
    }) ++ akkaTransitiveIds

  def getAkka210TestIvyModule = {
    val transitive = true
    val changing = true
    val force = true
    val ivyModule = DefaultModuleDescriptor.newBasicInstance(ModuleRevisionId.newInstance("com.adepthub", "test", "1.0"), new java.util.Date(1395315115209L))
    ivyModule.addConfiguration(new IvyConfiguration("default", IvyConfiguration.Visibility.PUBLIC, "", Array("master", "runtime"), true, ""))
    ivyModule.addConfiguration(new IvyConfiguration("master", IvyConfiguration.Visibility.PUBLIC, "", Array.empty, true, ""))
    ivyModule.addConfiguration(new IvyConfiguration("runtime", IvyConfiguration.Visibility.PUBLIC, "", Array("compile"), true, ""))
    ivyModule.addConfiguration(new IvyConfiguration("compile", IvyConfiguration.Visibility.PUBLIC, "", Array.empty, true, ""))
    ivyModule.addConfiguration(new IvyConfiguration("test", IvyConfiguration.Visibility.PRIVATE, "", Array("runtime"), true, ""))

    val akkaDep = new DefaultDependencyDescriptor(ivyModule,
      ModuleRevisionId.newInstance("com.typesafe.akka", "akka-actor_2.10", "2.1.0"), force, changing, transitive)
    akkaDep.addDependencyConfiguration("compile", "default(compile)")
    ivyModule.addDependency(akkaDep)
    ivyModule
  }

  def getAkka221TestIvyModule = {
    val transitive = true
    val changing = true
    val force = true
    val ivyModule = DefaultModuleDescriptor.newBasicInstance(ModuleRevisionId.newInstance("com.adepthub", "test", "1.0"), new java.util.Date(1395315115209L))
    ivyModule.addConfiguration(new IvyConfiguration("default", IvyConfiguration.Visibility.PUBLIC, "", Array("master", "runtime"), true, ""))
    ivyModule.addConfiguration(new IvyConfiguration("master", IvyConfiguration.Visibility.PUBLIC, "", Array.empty, true, ""))
    ivyModule.addConfiguration(new IvyConfiguration("runtime", IvyConfiguration.Visibility.PUBLIC, "", Array("compile"), true, ""))
    ivyModule.addConfiguration(new IvyConfiguration("compile", IvyConfiguration.Visibility.PUBLIC, "", Array.empty, true, ""))
    ivyModule.addConfiguration(new IvyConfiguration("test", IvyConfiguration.Visibility.PRIVATE, "", Array("runtime"), true, ""))

    val akkaDep = new DefaultDependencyDescriptor(ivyModule,
      ModuleRevisionId.newInstance("com.typesafe.akka", "akka-actor_2.10", "2.2.1"), force, changing, transitive)
    akkaDep.addDependencyConfiguration("compile", "default(compile)")
    ivyModule.addDependency(akkaDep)
    ivyModule
  }

  private def verifyLocation(artifacts: Set[Artifact], ending: String) = {
    artifacts.foreach { artifact =>
      artifact.locations.toSeq match {
        case elem +: Nil => assert(elem.endsWith(ending), "Location: " + elem + " does not end with: '" + ending + "'")
        case _ => assert(false, "Did not find expected location ending with: '" + ending + "'. Found " + artifact.locations)
      }
    }
  }

  test("IvyImport basics: import of akka should yield correct results") {

    implicit val testDetails = TestDetails("Basic import akka 2.1.0")
    val ivyConverter = new IvyAdeptConverter(ivy)
    val ivyModule = getAkka210TestIvyModule
    val results = benchmark(IvyImport, ivyModule) {
      ivyConverter.loadAsIvyImportResults(ivyModule, progress).failOnLeft
    }
    val byIds = results.groupBy(_.variant.id)
    byIds.keySet.intersect(akkaTransitiveIdsExpectedIds) should have size (akkaTransitiveIdsExpectedIds.size)

    results.foreach {
      case result =>
        if (result.variant.id == withConfiguration("com.typesafe.akka/akka-actor_2.10", "master")) {
          verifyLocation(result.artifacts, "akka-actor_2.10-2.1.0.jar")
        }
        if (result.variant.id == withConfiguration("com.typesafe/config", "master")) {
          verifyLocation(result.artifacts, "config-1.0.0.jar")
        }
        if (result.variant.id == withConfiguration("org.scala-lang/scala-library", "master")) {
          verifyLocation(result.artifacts, "scala-library-2.10.0.jar")
        }
        //
        if (result.variant.id == withConfiguration("com.typesafe.akka/akka-actor_2.10", "compile")) {
          result.variant.requirements.map(_.id) shouldEqual Set(Id("org.scala-lang/scala-library"), withConfiguration("org.scala-lang/scala-library", "compile"), withConfiguration("org.scala-lang/scala-library", "master"), Id("com.typesafe/config"), withConfiguration("com.typesafe/config", "compile"), withConfiguration("com.typesafe/config", "master"))
          result.repository shouldEqual RepositoryName("com.typesafe.akka")
          result.versionInfo.map { case (name, _, version) => name -> version } shouldEqual Set(
            (RepositoryName("org.scala-lang"), Version("2.10.0")),
            (RepositoryName("com.typesafe"), Version("1.0.0")))
        }
    }
  }

  test("IvyImport end-to-end: import of akka should resolve correctly") {
    implicit val testDetails = TestDetails("End-to-end (akka-remote & scalatest)")
    usingTmpDir { tmpDir =>
      val ivyConverter = new IvyAdeptConverter(ivy)
      val ivyModule = getAkka210TestIvyModule
      val results = benchmark(IvyImport, ivyModule) {
        ivyConverter.loadAsIvyImportResults(ivyModule, progress).failOnLeft
      }

      val resolutionResults = benchmark(Inserted, results) {
        IvyImportResultInserter.insertAsResolutionResults(tmpDir, results, progress)
      }

      //insert something else to make sure process is stable:
      {
        val ivyModule = getAkka221TestIvyModule
        val extraResults = benchmark(IvyImport, ivyModule) {
          ivyConverter.loadAsIvyImportResults(ivyModule, progress).failOnLeft
        }
        benchmark(Inserted, extraResults) {
          IvyImportResultInserter.insertAsResolutionResults(tmpDir, extraResults, progress)
        }
      }
      //update to latest commit to make sure process is stable:
      val confuscatedResolutionResults = resolutionResults.map { r =>
        r.copy(commit = (new GitRepository(tmpDir, r.repository)).getHead)
      }

      val loader = benchmark(IvyImport, confuscatedResolutionResults) {
        new GitLoader(tmpDir, confuscatedResolutionResults, progress, cacheManager)
      }
      val requirements = Set(
        Requirement("com.typesafe.akka/akka-actor_2.10", Set.empty, Set.empty),
        Requirement(withConfiguration("com.typesafe.akka/akka-actor_2.10", "compile"), Set.empty, Set.empty),
        Requirement(withConfiguration("com.typesafe.akka/akka-actor_2.10", "master"), Set.empty, Set.empty))
      val result = benchmark(Resolved, requirements && loader) {
        resolve(requirements, loader)
      }
      checkResolved(result, Set[Id](
        "com.typesafe/config/config/master",
        "org.scala-lang/scala-library/config/compile",
        "com.typesafe/config/config/compile",
        "com.typesafe.akka/akka-actor_2.10",
        "com.typesafe.akka/akka-actor_2.10/config/compile",
        "com.typesafe/config",
        "com.typesafe.akka/akka-actor_2.10/config/master",
        "org.scala-lang/scala-library",
        "org.scala-lang/scala-library/config/master"))
      checkAttributeVariants(result, "com.typesafe.akka/akka-actor_2.10", version -> Set("2.1.0"))
      checkAttributeVariants(result, "com.typesafe/config", version -> Set("1.0.0"))
      checkAttributeVariants(result, "org.scala-lang/scala-library", version -> Set("2.10.0"))
      //artifacts:
      checkArtifactFilename(new GitRepository(tmpDir, RepositoryName("com.typesafe.akka")), result.state.resolvedVariants("com.typesafe.akka/akka-actor_2.10/config/master"))
      checkArtifactFilename(new GitRepository(tmpDir, RepositoryName("com.typesafe")), result.state.resolvedVariants("com.typesafe/config/config/master"))
      checkArtifactFilename(new GitRepository(tmpDir, RepositoryName("org.scala-lang")), result.state.resolvedVariants("org.scala-lang/scala-library/config/master"))
      //artifact cache:
      checkArtifactCache(tmpDir, result.state.resolvedVariants("com.typesafe.akka/akka-actor_2.10/config/master"))
      checkArtifactCache(tmpDir, result.state.resolvedVariants("com.typesafe/config/config/master"))
      checkArtifactCache(tmpDir, result.state.resolvedVariants("org.scala-lang/scala-library/config/master"))
    }
  }

  def getAkkaRemoteTestIvyModule = {
    val transitive = true
    val changing = true
    val force = true
    val ivyModule = DefaultModuleDescriptor.newBasicInstance(ModuleRevisionId.newInstance("com.adepthub", "test", "1.0"), new java.util.Date(1395315115209L))
    ivyModule.addConfiguration(new IvyConfiguration("default", IvyConfiguration.Visibility.PUBLIC, "", Array("master", "runtime"), true, ""))
    ivyModule.addConfiguration(new IvyConfiguration("master", IvyConfiguration.Visibility.PUBLIC, "", Array.empty, true, ""))
    ivyModule.addConfiguration(new IvyConfiguration("runtime", IvyConfiguration.Visibility.PUBLIC, "", Array("compile"), true, ""))
    ivyModule.addConfiguration(new IvyConfiguration("compile", IvyConfiguration.Visibility.PUBLIC, "", Array.empty, true, ""))
    ivyModule.addConfiguration(new IvyConfiguration("test", IvyConfiguration.Visibility.PRIVATE, "", Array("runtime"), true, ""))

    val akkaDep = new DefaultDependencyDescriptor(ivyModule,
      ModuleRevisionId.newInstance("com.typesafe.akka", "akka-remote_2.10", "2.2.1"), force, changing, transitive)
    akkaDep.addExcludeRule("compile", new DefaultExcludeRule(new ArtifactId(new ModuleId("com.google.protobuf", "protobuf-java"), "*", "*", "*"), ExactPatternMatcher.INSTANCE, new java.util.HashMap()))
    akkaDep.addDependencyConfiguration("compile", "default(compile)")
    ivyModule.addDependency(akkaDep)
    val scalaTestDep = new DefaultDependencyDescriptor(ivyModule,
      ModuleRevisionId.newInstance("org.scalatest", "scalatest_2.10", "1.9.1"), force, changing, transitive)
    scalaTestDep.addDependencyConfiguration("test", "default(compile)")
    ivyModule.addDependency(scalaTestDep)
    ivyModule
  }

  test("Ivy requirements conversion") {
    val ivyModule = getAkkaRemoteTestIvyModule

    val fakeIvyResults: Set[IvyImportResult] = Set(
      IvyImportResult(
        variant = (Variant(
          id = Id("akka-remote_2.10/config/default"),
          attributes = Set(
            Attribute(ConfigurationAttribute, Set("default")),
            Attribute(IvyNameAttribute, Set("akka-remote_2.10")),
            Attribute(VersionAttribute, Set("2.2.1")),
            Attribute(IvyOrgAttribute, Set("com.typesafe.akka"))))),
        repository = RepositoryName("com.typesafe.akka"),
        artifacts = Set.empty, localFiles = Map.empty,
        versionInfo = Set.empty, excludeRules = Map.empty),
      IvyImportResult(
        variant = (Variant(
          id = Id("akka-remote_2.10/config/compile"),
          attributes = Set(
            Attribute(ConfigurationAttribute, Set("compile")),
            Attribute(IvyNameAttribute, Set("akka-remote_2.10")),
            Attribute(VersionAttribute, Set("2.2.1")),
            Attribute(IvyOrgAttribute, Set("com.typesafe.akka"))))),
        repository = RepositoryName("com.typesafe.akka"),
        artifacts = Set.empty, localFiles = Map.empty,
        versionInfo = Set.empty, excludeRules = Map.empty),
      IvyImportResult(
        variant = (Variant(
          id = Id("akka-remote_2.10/config/runtime"),
          attributes = Set(
            Attribute(ConfigurationAttribute, Set("runtime")),
            Attribute(IvyNameAttribute, Set("akka-remote_2.10")),
            Attribute(VersionAttribute, Set("2.2.1")),
            Attribute(IvyOrgAttribute, Set("com.typesafe.akka"))))),
        repository = RepositoryName("com.typesafe.akka"),
        artifacts = Set.empty, localFiles = Map.empty,
        versionInfo = Set.empty, excludeRules = Map.empty),
      IvyImportResult(
        variant = (Variant(
          id = Id("akka-remote_2.10/config/master"),
          attributes = Set(
            Attribute(ConfigurationAttribute, Set("master")),
            Attribute(IvyNameAttribute, Set("akka-remote_2.10")),
            Attribute(VersionAttribute, Set("2.2.1")),
            Attribute(IvyOrgAttribute, Set("com.typesafe.akka"))))),
        repository = RepositoryName("com.typesafe.akka"),
        artifacts = Set.empty, localFiles = Map.empty,
        versionInfo = Set.empty, excludeRules = Map.empty),
      //BOGUS:
      IvyImportResult(
        variant = (Variant(
          id = Id("akka-remote_2.10/config/bogus"),
          attributes = Set(
            Attribute(ConfigurationAttribute, Set("bogus")),
            Attribute(IvyNameAttribute, Set("akka-remote_2.10")),
            Attribute(VersionAttribute, Set("2.2.1")),
            Attribute(IvyOrgAttribute, Set("com.typesafe.akka"))))),
        repository = RepositoryName("com.typesafe.akka"),
        artifacts = Set.empty, localFiles = Map.empty,
        versionInfo = Set.empty, excludeRules = Map.empty),
      //----SCALATEST
      IvyImportResult(
        variant = (Variant(
          id = Id("scalatest_2.10/config/default"),
          attributes = Set(
            Attribute(ConfigurationAttribute, Set("default")),
            Attribute(IvyNameAttribute, Set("scalatest_2.10")),
            Attribute(VersionAttribute, Set("1.9.1")),
            Attribute(IvyOrgAttribute, Set("org.scalatest"))))),
        repository = RepositoryName("org.scalatest"),
        artifacts = Set.empty, localFiles = Map.empty,
        versionInfo = Set.empty, excludeRules = Map.empty),
      IvyImportResult(
        variant = (Variant(
          id = Id("scalatest_2.10/config/compile"),
          attributes = Set(
            Attribute(ConfigurationAttribute, Set("compile")),
            Attribute(IvyNameAttribute, Set("scalatest_2.10")),
            Attribute(VersionAttribute, Set("1.9.1")),
            Attribute(IvyOrgAttribute, Set("org.scalatest"))))),
        repository = RepositoryName("org.scalatest"),
        artifacts = Set.empty, localFiles = Map.empty,
        versionInfo = Set.empty, excludeRules = Map.empty),
      IvyImportResult(
        variant = (Variant(
          id = Id("scalatest_2.10/config/runtime"),
          attributes = Set(
            Attribute(ConfigurationAttribute, Set("runtime")),
            Attribute(IvyNameAttribute, Set("scalatest_2.10")),
            Attribute(VersionAttribute, Set("1.9.1")),
            Attribute(IvyOrgAttribute, Set("org.scalatest"))))),
        repository = RepositoryName("org.scalatest"),
        artifacts = Set.empty, localFiles = Map.empty,
        versionInfo = Set.empty, excludeRules = Map.empty),
      IvyImportResult(
        variant = (Variant(
          id = Id("scalatest_2.10/config/master"),
          attributes = Set(
            Attribute(ConfigurationAttribute, Set("master")),
            Attribute(IvyNameAttribute, Set("scalatest_2.10")),
            Attribute(VersionAttribute, Set("1.9.1")),
            Attribute(IvyOrgAttribute, Set("org.scalatest"))))),
        repository = RepositoryName("org.scalatest"),
        artifacts = Set.empty, localFiles = Map.empty,
        versionInfo = Set.empty, excludeRules = Map.empty))

    val requirements = IvyRequirements.convertIvyAsRequirements(ivyModule, fakeIvyResults)
    requirements("compile") shouldEqual Set(
      Requirement(Id("akka-remote_2.10/config/default"), Set.empty, Set.empty),
      Requirement(Id("akka-remote_2.10/config/master"), Set.empty, Set.empty),
      Requirement(Id("akka-remote_2.10/config/compile"), Set.empty, Set.empty),
      Requirement(Id("akka-remote_2.10/config/runtime"), Set.empty, Set.empty))

    requirements("test") shouldEqual Set(
      Requirement(Id("akka-remote_2.10/config/default"), Set.empty, Set.empty),
      Requirement(Id("akka-remote_2.10/config/master"), Set.empty, Set.empty),
      Requirement(Id("akka-remote_2.10/config/compile"), Set.empty, Set.empty),
      Requirement(Id("akka-remote_2.10/config/runtime"), Set.empty, Set.empty),
      Requirement(Id("scalatest_2.10/config/default"), Set.empty, Set.empty),
      Requirement(Id("scalatest_2.10/config/master"), Set.empty, Set.empty),
      Requirement(Id("scalatest_2.10/config/compile"), Set.empty, Set.empty),
      Requirement(Id("scalatest_2.10/config/runtime"), Set.empty, Set.empty))
  }

  test("Ivy end-to-end: import, insert, resolution, verification") {
    implicit val testDetails = TestDetails("End-to-end (akka-remote & scalatest)")
    usingTmpDir { tmpDir =>
      ivy.configure(new File("src/test/resources/typesafe-ivy-settings.xml"))

      val ivyConverter = new IvyAdeptConverter(ivy)
      val ivyModule = getAkkaRemoteTestIvyModule

      val results = benchmark(IvyImport, ivyModule) {
        ivyConverter.loadAsIvyImportResults(ivyModule, progress).failOnLeft
      }
      val resolutionResults = benchmark(Inserted, results) {
        IvyImportResultInserter.insertAsResolutionResults(tmpDir, results, progress)
      }

      val requirements = benchmark(Converted, ivyModule && results) {
        IvyRequirements.convertIvyAsRequirements(ivyModule, results)
      }

      val loader = benchmark(Loaded, resolutionResults) {
        new GitLoader(tmpDir, resolutionResults, progress, cacheManager)
      }

      for (confName <- requirements.keys) {
        println(confName)
        val result = benchmark(Resolved, requirements(confName) && loader) {
          resolve(requirements(confName), loader)
        }
        result match {
          case resolvedResult: ResolvedResult =>
            val verificationResult = benchmark(Verified, resolvedResult && ivyModule) {
              ivyConverter.verifyConversion(confName, ivyModule, resolvedResult)
            }
            assert(verificationResult.isRight, "Verification of " + confName + " failed:\n" + verificationResult)
            
            val lockfile = Lockfile.create(tmpDir, requirements(confName), resolutionResults, result, cacheManager)
            println(lockfile.jsonString)
            import scala.concurrent.duration._
            Lockfile.download(tmpDir, lockfile, 1.minute, progress)
            
          case _ =>
            assert(false, "Expected to be able to resolve Adept for " + confName + ". Got result:\n" + result)
        }
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