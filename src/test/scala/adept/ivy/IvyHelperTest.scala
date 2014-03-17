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

  test("IvyImport basics: import of akka should yield correct results") {
    import IvyHelper._
    val ivy = IvyHelper.load()
    val ivyHelper = new IvyHelper(ivy)

    val results = ivyHelper.ivyImport("com.typesafe.akka", "akka-actor_2.10", "2.1.0")

    val justIds: Set[Id] = Set(
      "akka-actor_2.10",
      "scala-library",
      "config")
    val expectedIds =
      (for {
        id <- justIds
        confName <- Set("optional", "test", "runtime", "provided", "javadoc", "system", "default", "sources", "compile")
      } yield {
        withConfiguration(id, confName)
      }) ++ justIds

    val byIds = results.groupBy(_.variant.id)
    byIds.keySet.intersect(expectedIds) should have size (expectedIds.size)

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
          result.versionInfo shouldEqual Set(
            (RepositoryName("org.scala-lang"), Id("scala-library"), Version("2.10.0")),
            (RepositoryName("com.typesafe"), Id("config"), Version("1.0.0")))
        }
    }
  }
}