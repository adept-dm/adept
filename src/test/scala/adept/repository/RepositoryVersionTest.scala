package adept.repository

import org.scalatest._
import adept.ivy._
import adept.core.models._
import java.io.File
import org.apache.ivy.core.module.id.ModuleRevisionId
import adept.ext.Version

class RepositoryVersionTest extends FunSuite with Matchers {

  test("repo version basic tests") {
    import EitherValues._

    val ivy = IvyHelper.load().right.value
    val ivyHelper = new IvyHelper(ivy)

    val baseDir = new File("test-dir")

    //    IvyHelper.insert(Set(IvyImportResult(ModuleRevisionId.newInstance("com.typesafe.akka", "akka-actors", "2.2.1"), variants = Set(
    //      Variant(Id("akka-actors"), attributes = Set(Attribute("version", Set("2.2.1"))), artifacts = Set.empty, dependencies = Set.empty)), artifacts = Set.empty, localFiles = Map.empty)), baseDir)
    //
    //    IvyHelper.insert(Set(IvyImportResult(ModuleRevisionId.newInstance("com.typesafe.akka", "akka-actors", "2.2.0"), variants = Set(
    //      Variant(Id("akka-actors"), attributes = Set(Attribute("version", Set("2.2.0"))), artifacts = Set.empty, dependencies = Set.empty)), artifacts = Set.empty, localFiles = Map.empty)), baseDir)

    import scala.reflect.io.Directory
    (new Directory(baseDir)).deleteRecursively()

    AdeptRepositoryManager.init(baseDir, "wedge-test")
    val v205 = Variant(Id("akka-actors"), attributes = Set(Attribute("version", Set("2.0.5"))), artifacts = Set.empty, dependencies = Set.empty)

    val repo = new LocalGitRepository(baseDir, name = "wedge-test", Commit.Head)
    
    repo.writeVariant(v205)
    repo.commit("2.0.5 release")
    val v220 = Variant(Id("akka-actors"), attributes = Set(Attribute("version", Set("2.2.0"))), artifacts = Set.empty, dependencies = Set.empty)
    repo.deleteVariant(v205)
    repo.writeVariant(v220)
    repo.commit("2.2.0 release")
    repo.deleteVariant(v220)
    repo.writeVariant(Variant(Id("akka-actors"), attributes = Set(Attribute("version", Set("2.2.2"))), artifacts = Set.empty, dependencies = Set.empty))
    repo.commit("2.2.2 release")

    def getVersion(variant: Variant): Option[Version] = {
      val versionValues = variant.attribute("version").values
      if (versionValues.size > 1) {
        println("Found multiple versions for: " + variant) //TODO: logger.warn
        None
      } else {
        versionValues.headOption.map(Version.apply _)
      }
    }
    val variant = Variant(Id("akka-actors"), attributes = Set(Attribute("version", Set("2.2.1"))), artifacts = Set.empty, dependencies = Set.empty)
    val version = getVersion(variant)
    val commit = repo.scan(Id("akka-actors")) { currentVariant =>
      getVersion(currentVariant) < version
    }.get
    println(commit)

    repo.wedge(variant, commit)

    //
    //    val ivyResults1 = ivyHelper.ivyImport("com.typesafe.akka", "akka-actor_2.10", "2.2.1").right.value
    //    
    //    val repos1 = IvyHelper.insert(ivyResults1, baseDir)
    //    //...
    //    val ivyResults2 = ivyHelper.ivyImport("com.typesafe.akka", "akka-actor_2.10", "2.2.0").right.value
    //    val repos2 = IvyHelper.insert(ivyResults1, baseDir)
  }
}