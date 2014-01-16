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

    import scala.reflect.io.Directory
    (new Directory(baseDir)).deleteRecursively()

    IvyHelper.insert(Set(IvyImportResult(ModuleRevisionId.newInstance("com.typesafe.akka", "akka-actors", "2.2.1"), variants = Set(
      Variant(Id("akka-actors"), attributes = Set(Attribute("version", Set("2.2.1"))), artifacts = Set.empty, dependencies = Set.empty)), artifacts = Set.empty, localFiles = Map.empty)),
      baseDir)

    IvyHelper.insert(Set(IvyImportResult(ModuleRevisionId.newInstance("com.typesafe.akka", "akka-actors", "2.2.0"), variants = Set(
      Variant(Id("akka-actors"), attributes = Set(Attribute("version", Set("2.2.0"))), artifacts = Set.empty, dependencies = Set.empty)), artifacts = Set.empty, localFiles = Map.empty)),
      baseDir)

    IvyHelper.insert(Set(IvyImportResult(ModuleRevisionId.newInstance("com.typesafe.akka", "akka-actors", "2.0.5"), variants = Set(
      Variant(Id("akka-actors"), attributes = Set(Attribute("version", Set("2.0.5"))), artifacts = Set.empty, dependencies = Set.empty)), artifacts = Set.empty, localFiles = Map.empty)),
      baseDir)

//    IvyHelper.insert(Set(IvyImportResult(ModuleRevisionId.newInstance("com.typesafe.akka", "akka-actors", "2.2.3"), variants = Set(
//      Variant(Id("akka-actors"), attributes = Set(Attribute("version", Set("2.2.3"))), artifacts = Set.empty, dependencies = Set.empty)), artifacts = Set.empty, localFiles = Map.empty)),
//      baseDir)
    //
    //    val ivyResults1 = ivyHelper.ivyImport("com.typesafe.akka", "akka-actor_2.10", "2.2.1").right.value
    //    
    //    val repos1 = IvyHelper.insert(ivyResults1, baseDir)
    //    //...
    //    val ivyResults2 = ivyHelper.ivyImport("com.typesafe.akka", "akka-actor_2.10", "2.2.0").right.value
    //    val repos2 = IvyHelper.insert(ivyResults1, baseDir)
  }
}