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

  test("IvyImport basics: import of akka should yield correct results") {
    val ivy = IvyHelper.load()
    val ivyHelper = new IvyHelper(ivy)

    val results = ivyHelper.ivyConvert("com.typesafe.akka", "akka-actor_2.10", "2.1.0", progress)

    val byIds = results.groupBy(_.variant.id)
    byIds.keySet.intersect(akkaTransitiveIdsExpectedIds) should have size (akkaTransitiveIdsExpectedIds.size)

    results.foreach {
      case result =>
        if (result.variant.id == IvyHelper.withConfiguration("akka-actor_2.10", "master")) {
          result.artifacts.flatMap(_.locations) shouldEqual Set("http://repo1.maven.org/maven2/com/typesafe/akka/akka-actor_2.10/2.1.0/akka-actor_2.10-2.1.0.jar")
        }
        if (result.variant.id == IvyHelper.withConfiguration("config", "master")) {
          result.artifacts.flatMap(_.locations) shouldEqual Set("http://repo.typesafe.com/typesafe/releases/com/typesafe/config/1.0.0/config-1.0.0.jar")
        }
        if (result.variant.id == IvyHelper.withConfiguration("scala-library", "master")) {
          result.artifacts.flatMap(_.locations) shouldEqual Set("http://repo1.maven.org/maven2/org/scala-lang/scala-library/2.10.0/scala-library-2.10.0.jar")
        }
        //
        if (result.variant.id == IvyHelper.withConfiguration("akka-actor_2.10", "compile")) {
          result.variant.requirements.map(_.id) shouldEqual Set(Id("scala-library"), withConfiguration("scala-library", "compile"), withConfiguration("scala-library", "master"), Id("config"), withConfiguration("config", "compile"), withConfiguration("config", "master"))
          result.repository shouldEqual RepositoryName("com.typesafe.akka")
          result.versionInfo.map { case (name, _, version) => name -> version } shouldEqual Set(
            (RepositoryName("org.scala-lang"), Version("2.10.0")),
            (RepositoryName("com.typesafe"), Version("1.0.0")))
        }
    }
  }

  test("IvyImport end-to-end: import of akka should resolve correctly") {
    usingTmpDir { tmpDir =>
      import IvyHelper._
      val ivy = IvyHelper.load()
      val ivyHelper = new IvyHelper(ivy)

      val results = ivyHelper.ivyConvert("com.typesafe.akka", "akka-actor_2.10", "2.1.0", progress)

      val resolutionResults = IvyHelper.insert(tmpDir, results, progress)

      //insert something else to make sure process is stable:
      IvyHelper.insert(tmpDir, ivyHelper.ivyConvert("com.typesafe.akka", "akka-actor_2.10", "2.2.1", progress), progress)
      //update to latest commit to make sure process is stable:
      val confuscatedResolutionResults = resolutionResults.map { r =>
        r.copy(commit = (new GitRepository(tmpDir, r.repository)).getHead)
      }

      val loader = new GitLoader(tmpDir, confuscatedResolutionResults, progress, cacheManager)
      val requirements = Set(
        Requirement("akka-actor_2.10", Set.empty),
        Requirement(withConfiguration("akka-actor_2.10", "compile"), Set.empty),
        Requirement(withConfiguration("akka-actor_2.10", "master"), Set.empty))
      val result = resolve(requirements, loader)
      checkResolved(result, Set[Id](
        "config/config/master",
        "scala-library/config/compile",
        "config/config/compile",
        "akka-actor_2.10",
        "akka-actor_2.10/config/compile",
        "config",
        "akka-actor_2.10/config/master",
        "scala-library",
        "scala-library/config/master"))
      checkAttributeVariants(result, "akka-actor_2.10", version -> Set("2.1.0"))
      checkAttributeVariants(result, "config", version -> Set("1.0.0"))
      checkAttributeVariants(result, "scala-library", version -> Set("2.10.0"))
    }
  }

//  test("REMOVE ME: add a proper more complicated test") {
//    usingTmpDir { tmpDir =>
//      import IvyHelper._
//      val ivy = IvyHelper.load()
//      ivy.configure(new File("src/test/resources/typesafe-ivy-settings.xml"))
//
//      val ivyHelper = new IvyHelper(ivy)
//      val time1 = System.currentTimeMillis()
//      val results = ivyHelper.ivyConvert("com.typesafe.play", "play_2.10", "2.2.2", progress)
//      val time2 = System.currentTimeMillis()
//      println("import completed: " + ((time2 - time1)/1000.0) + "s")
//      val resolutionResults = IvyHelper.insert(tmpDir, results, progress)
//      val time3 = System.currentTimeMillis()
//      println("insert completed: " + ((time3 - time2)/1000.0) + "s")
//      val confuscatedResolutionResults = resolutionResults.map { r =>
//        r.copy(commit = (new GitRepository(tmpDir, r.repository)).getHead)
//      }
//
//      val loader = new GitLoader(tmpDir, confuscatedResolutionResults, progress, cacheManager)
//      val time4 = System.currentTimeMillis()
//      println("loaded in: " + ((time4 - time3)/1000.0) + "s")
//      val requirements = Set(
//        Requirement("play_2.10", Set.empty),
//        Requirement(withConfiguration("play_2.10", "compile"), Set.empty),
//        Requirement(withConfiguration("play_2.10", "master"), Set.empty))
//      val result = resolve(requirements, loader)
//      val time5 = System.currentTimeMillis()
//      println("resolution completed: " + ((time5 - time4)/1000.0) + "s")
//      println(result)
//    }
//  }

  test("IvyImport end-to-end: import of akka should yield correct classpath") {
    pending
  }
}