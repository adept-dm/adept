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
import adept.ext.VersionOrder
import adept.repository.serialization.Order
import adept.repository.serialization.VariantMetadata
import adept.utils.Hasher
import net.sf.ehcache.CacheManager
import org.eclipse.jgit.lib.TextProgressMonitor
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

class IvyHelperTest extends FunSuite with Matchers {
  import adept.test.ResolverUtils._
  import adept.test.LoaderUtils._
  import adept.test.FileUtils.usingTmpDir
  import IvyHelper._

  val akkaTransitiveIds: Set[Id] = Set(
    "akka-actor_2.10",
    "scala-library",
    "config")
  val akkaTransitiveIdsExpectedIds =
    (for {
      id <- akkaTransitiveIds
      confName <- Set("optional", "test", "runtime", "provided", "javadoc", "system", "default", "sources", "compile")
    } yield {
      withConfiguration(id, confName)
    }) ++ akkaTransitiveIds
  //
  //  test("IvyImport basics: import of akka should yield correct results") {
  //    val ivy = IvyHelper.load()
  //    val ivyHelper = new IvyHelper(ivy)
  //    val results = ivyHelper.ivyImport("com.typesafe.akka", "akka-actor_2.10", "2.1.0", progress)
  //
  //    val byIds = results.groupBy(_.variant.id)
  //    byIds.keySet.intersect(akkaTransitiveIdsExpectedIds) should have size (akkaTransitiveIdsExpectedIds.size)
  //
  //    results.foreach {
  //      case result =>
  //        if (result.variant.id == IvyHelper.withConfiguration("akka-actor_2.10", "master")) {
  //          result.artifacts.flatMap(_.locations) shouldEqual Set("http://repo1.maven.org/maven2/com/typesafe/akka/akka-actor_2.10/2.1.0/akka-actor_2.10-2.1.0.jar")
  //        }
  //        if (result.variant.id == IvyHelper.withConfiguration("config", "master")) {
  //          result.artifacts.flatMap(_.locations) shouldEqual Set("http://repo.typesafe.com/typesafe/releases/com/typesafe/config/1.0.0/config-1.0.0.jar")
  //        }
  //        if (result.variant.id == IvyHelper.withConfiguration("scala-library", "master")) {
  //          result.artifacts.flatMap(_.locations) shouldEqual Set("http://repo1.maven.org/maven2/org/scala-lang/scala-library/2.10.0/scala-library-2.10.0.jar")
  //        }
  //        //
  //        if (result.variant.id == IvyHelper.withConfiguration("akka-actor_2.10", "compile")) {
  //          result.variant.requirements.map(_.id) shouldEqual Set(Id("scala-library"), withConfiguration("scala-library", "compile"), withConfiguration("scala-library", "master"), Id("config"), withConfiguration("config", "compile"), withConfiguration("config", "master"))
  //          result.repository shouldEqual RepositoryName("com.typesafe.akka")
  //          result.versionInfo.map { case (name, _, version) => name -> version } shouldEqual Set(
  //            (RepositoryName("org.scala-lang"), Version("2.10.0")),
  //            (RepositoryName("com.typesafe"), Version("1.0.0")))
  //        }
  //    }
  //  }
  //
  //  test("IvyImport end-to-end: import of akka should resolve correctly") {
  //    usingTmpDir { tmpDir =>
  //      val ivy = IvyHelper.load()
  //      val ivyHelper = new IvyHelper(ivy)
  //
  //      val results = ivyHelper.ivyImport("com.typesafe.akka", "akka-actor_2.10", "2.1.0", progress)
  //
  //      val resolutionResults = IvyHelper.insert(tmpDir, results, progress)
  //
  //      //insert something else to make sure process is stable:
  //      IvyHelper.insert(tmpDir, ivyHelper.ivyImport("com.typesafe.akka", "akka-actor_2.10", "2.2.1", progress), progress)
  //      //update to latest commit to make sure process is stable:
  //      val confuscatedResolutionResults = resolutionResults.map { r =>
  //        r.copy(commit = (new GitRepository(tmpDir, r.repository)).getHead)
  //      }
  //
  //      val loader = new GitLoader(tmpDir, confuscatedResolutionResults, progress, cacheManager)
  //      val requirements = Set(
  //        Requirement("akka-actor_2.10", Set.empty),
  //        Requirement(withConfiguration("akka-actor_2.10", "compile"), Set.empty),
  //        Requirement(withConfiguration("akka-actor_2.10", "master"), Set.empty))
  //      val result = resolve(requirements, loader)
  //      checkResolved(result, Set[Id](
  //        "config/config/master",
  //        "scala-library/config/compile",
  //        "config/config/compile",
  //        "akka-actor_2.10",
  //        "akka-actor_2.10/config/compile",
  //        "config",
  //        "akka-actor_2.10/config/master",
  //        "scala-library",
  //        "scala-library/config/master"))
  //      checkAttributeVariants(result, "akka-actor_2.10", version -> Set("2.1.0"))
  //      checkAttributeVariants(result, "config", version -> Set("1.0.0"))
  //      checkAttributeVariants(result, "scala-library", version -> Set("2.10.0"))
  //    }
  //  }

  def getAkkaRemoteTestIvyModule = {
    val transitive = true
    val changing = true
    val force = true
    val ivyModule = DefaultModuleDescriptor.newBasicInstance(ModuleRevisionId.newInstance("com.adepthub", "test", "1.0"), new java.util.Date())
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

  test("Ivy requirements transformation") {
    import adept.ext.AttributeDefaults._
    val ivyModule = getAkkaRemoteTestIvyModule

    val fakeIvyResults: Set[IvyImportResult] = Set(
      IvyImportResult(
        variant = (Variant(
          id = Id("akka-remote_2.10/config/default"),
          attributes = Set(
            Attribute(ConfigurationAttribute, Set("default")),
            Attribute(NameAttribute, Set("akka-remote_2.10")),
            Attribute(VersionAttribute, Set("2.2.1")),
            Attribute(OrgAttribute, Set("com.typesafe.akka"))))),
        repository = RepositoryName("com.typesafe.akka"),
        artifacts = Set.empty, localFiles = Map.empty,
        versionInfo = Set.empty, excludeRules = Map.empty),
      IvyImportResult(
        variant = (Variant(
          id = Id("akka-remote_2.10/config/compile"),
          attributes = Set(
            Attribute(ConfigurationAttribute, Set("compile")),
            Attribute(NameAttribute, Set("akka-remote_2.10")),
            Attribute(VersionAttribute, Set("2.2.1")),
            Attribute(OrgAttribute, Set("com.typesafe.akka"))))),
        repository = RepositoryName("com.typesafe.akka"),
        artifacts = Set.empty, localFiles = Map.empty,
        versionInfo = Set.empty, excludeRules = Map.empty),
      IvyImportResult(
        variant = (Variant(
          id = Id("akka-remote_2.10/config/runtime"),
          attributes = Set(
            Attribute(ConfigurationAttribute, Set("runtime")),
            Attribute(NameAttribute, Set("akka-remote_2.10")),
            Attribute(VersionAttribute, Set("2.2.1")),
            Attribute(OrgAttribute, Set("com.typesafe.akka"))))),
        repository = RepositoryName("com.typesafe.akka"),
        artifacts = Set.empty, localFiles = Map.empty,
        versionInfo = Set.empty, excludeRules = Map.empty),
      IvyImportResult(
        variant = (Variant(
          id = Id("akka-remote_2.10/config/master"),
          attributes = Set(
            Attribute(ConfigurationAttribute, Set("master")),
            Attribute(NameAttribute, Set("akka-remote_2.10")),
            Attribute(VersionAttribute, Set("2.2.1")),
            Attribute(OrgAttribute, Set("com.typesafe.akka"))))),
        repository = RepositoryName("com.typesafe.akka"),
        artifacts = Set.empty, localFiles = Map.empty,
        versionInfo = Set.empty, excludeRules = Map.empty),
      //BOGUS:
      IvyImportResult(
        variant = (Variant(
          id = Id("akka-remote_2.10/config/bogus"),
          attributes = Set(
            Attribute(ConfigurationAttribute, Set("bogus")),
            Attribute(NameAttribute, Set("akka-remote_2.10")),
            Attribute(VersionAttribute, Set("2.2.1")),
            Attribute(OrgAttribute, Set("com.typesafe.akka"))))),
        repository = RepositoryName("com.typesafe.akka"),
        artifacts = Set.empty, localFiles = Map.empty,
        versionInfo = Set.empty, excludeRules = Map.empty),
      //----SCALATEST
      IvyImportResult(
        variant = (Variant(
          id = Id("scalatest_2.10/config/default"),
          attributes = Set(
            Attribute(ConfigurationAttribute, Set("default")),
            Attribute(NameAttribute, Set("scalatest_2.10")),
            Attribute(VersionAttribute, Set("1.9.1")),
            Attribute(OrgAttribute, Set("org.scalatest"))))),
        repository = RepositoryName("org.scalatest"),
        artifacts = Set.empty, localFiles = Map.empty,
        versionInfo = Set.empty, excludeRules = Map.empty),
      IvyImportResult(
        variant = (Variant(
          id = Id("scalatest_2.10/config/compile"),
          attributes = Set(
            Attribute(ConfigurationAttribute, Set("compile")),
            Attribute(NameAttribute, Set("scalatest_2.10")),
            Attribute(VersionAttribute, Set("1.9.1")),
            Attribute(OrgAttribute, Set("org.scalatest"))))),
        repository = RepositoryName("org.scalatest"),
        artifacts = Set.empty, localFiles = Map.empty,
        versionInfo = Set.empty, excludeRules = Map.empty),
      IvyImportResult(
        variant = (Variant(
          id = Id("scalatest_2.10/config/runtime"),
          attributes = Set(
            Attribute(ConfigurationAttribute, Set("runtime")),
            Attribute(NameAttribute, Set("scalatest_2.10")),
            Attribute(VersionAttribute, Set("1.9.1")),
            Attribute(OrgAttribute, Set("org.scalatest"))))),
        repository = RepositoryName("org.scalatest"),
        artifacts = Set.empty, localFiles = Map.empty,
        versionInfo = Set.empty, excludeRules = Map.empty),
      IvyImportResult(
        variant = (Variant(
          id = Id("scalatest_2.10/config/master"),
          attributes = Set(
            Attribute(ConfigurationAttribute, Set("master")),
            Attribute(NameAttribute, Set("scalatest_2.10")),
            Attribute(VersionAttribute, Set("1.9.1")),
            Attribute(OrgAttribute, Set("org.scalatest"))))),
        repository = RepositoryName("org.scalatest"),
        artifacts = Set.empty, localFiles = Map.empty,
        versionInfo = Set.empty, excludeRules = Map.empty))

    val requirements = IvyHelper.convertIvyAsRequirements(ivyModule, fakeIvyResults)
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

  test("REMOVE ME: add a proper more complicated test") {
    usingTmpDir { tmpDir =>
      import IvyHelper._
      import org.scalatest.EitherValues._
      val ivy = IvyHelper.load()
      ivy.configure(new File("src/test/resources/typesafe-ivy-settings.xml"))

      val ivyHelper = new IvyHelper(ivy)
      val ivyModule = getAkkaRemoteTestIvyModule

      val time1 = System.currentTimeMillis()
      val results = ivyHelper.getIvyImportResults(ivyModule, progress)
      val time2 = System.currentTimeMillis()
      println("import completed: " + ((time2 - time1) / 1000.0) + "s")
      val resolutionResults = IvyHelper.insertAsResolutionResults(tmpDir, results, progress)
      val time3 = System.currentTimeMillis()
      println("insert completed: " + ((time3 - time2) / 1000.0) + "s")
      val requirements = IvyHelper.convertIvyAsRequirements(ivyModule, results)
      val loader = new GitLoader(tmpDir, resolutionResults, progress, cacheManager)
      for (conf <- List("test", "compile")) {
        val time4 = System.currentTimeMillis()
        println("loaded " + conf + " in: " + ((time4 - time3) / 1000.0) + "s")
        val result = resolve(requirements(conf), loader)
        val time5 = System.currentTimeMillis()
        println("resolution in " + conf + " completed: " + ((time5 - time4) / 1000.0) + "s")
        result match {
          case resolvedResult: ResolvedResult =>
            val verificationResult  = ivyHelper.verifyImport(conf, ivyModule, resolvedResult)
            if (verificationResult.isRight) {
              println("verification of " + conf + " completed: " + ((time5 - time4) / 1000.0) + "s")
            } else {
              assert(false, "Verification of "+conf+" failed:\n" + verificationResult)
            }
          case _ =>
            assert(false, "Expected to be able to resolve Adept for " + conf + ". Got result:\n" + result)
        }
      }
    }
  }

  test("IvyImport end-to-end: import of akka should yield correct classpath") {
    pending
  }
}