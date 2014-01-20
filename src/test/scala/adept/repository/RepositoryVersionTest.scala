package adept.repository

import org.scalatest._
import adept.ivy._
import adept.core.models._
import java.io.File
import org.apache.ivy.core.module.id.ModuleRevisionId
import adept.ext.Version
import org.eclipse.jgit.api.Git
import java.io.FileOutputStream

class RepositoryVersionTest extends FunSuite with Matchers {

  def pattern(dir: File, file: File) = {

  }

  def createFile(dir: File): String = {
    val f = new File(dir, System.currentTimeMillis().toString)
    val fos = new FileOutputStream(f)
    try {
      fos.write("random stff\n".getBytes)
      f.getAbsolutePath().replace(dir.getAbsolutePath() + "/", "")
    } finally {
      fos.close()
    }
  }

  test("repo version basic tests") {
    import EitherValues._

    val ivy = IvyHelper.load().right.value
    val ivyHelper = new IvyHelper(ivy)

    val baseDir = new File("test-dir")

    import scala.reflect.io.Directory
    (new Directory(baseDir)).deleteRecursively()

    IvyHelper.insert(Set(IvyImportResult(ModuleRevisionId.newInstance("com.typesafe.akka", "akka-actors", "2.2.0"), variants = Set(
      Variant(Id("akka-actors"), attributes = Set(Attribute("version", Set("2.2.0"))), artifacts = Set.empty, dependencies = Set.empty)), artifacts = Set.empty, localFiles = Map.empty)),
      baseDir)

    IvyHelper.insert(Set(IvyImportResult(ModuleRevisionId.newInstance("com.typesafe.akka", "akka-actors", "2.2.1"), variants = Set(
      Variant(Id("akka-actors"), attributes = Set(Attribute("version", Set("2.2.1"))), artifacts = Set.empty, dependencies = Set.empty)), artifacts = Set.empty, localFiles = Map.empty)),
      baseDir)

    IvyHelper.insert(Set(IvyImportResult(ModuleRevisionId.newInstance("com.typesafe.akka", "akka-actors", "2.0.5"), variants = Set(
      Variant(Id("akka-actors"), attributes = Set(Attribute("version", Set("2.0.5"))), artifacts = Set.empty, dependencies = Set.empty)), artifacts = Set.empty, localFiles = Map.empty)),
      baseDir)
      
    IvyHelper.insert(Set(IvyImportResult(ModuleRevisionId.newInstance("com.typesafe.akka", "akka-actors", "2.0.4"), variants = Set(
      Variant(Id("akka-actors"), attributes = Set(Attribute("version", Set("2.0.4"))), artifacts = Set.empty, dependencies = Set.empty)), artifacts = Set.empty, localFiles = Map.empty)),
      baseDir)
      
    IvyHelper.insert(Set(IvyImportResult(ModuleRevisionId.newInstance("com.typesafe.akka", "akka-actors", "2.2.3"), variants = Set(
      Variant(Id("akka-actors"), attributes = Set(Attribute("version", Set("2.2.3"))), artifacts = Set.empty, dependencies = Set.empty)), artifacts = Set.empty, localFiles = Map.empty)),
      baseDir)

    IvyHelper.insert(Set(IvyImportResult(ModuleRevisionId.newInstance("com.typesafe.akka", "akka-actors", "2.2.2"), variants = Set(
      Variant(Id("akka-actors"), attributes = Set(Attribute("version", Set("2.2.2"))), artifacts = Set.empty, dependencies = Set.empty)), artifacts = Set.empty, localFiles = Map.empty)),
      baseDir)
      
    val lgr = new LocalGitRepository(baseDir, "com.typesafe.akka", Commit.Head)  

    
    val res1 = lgr.scan(Id("akka-actors")){ v =>
      val versionString = v.attribute("version").values.head
      (versionString == "2.0.5")
    }.get < lgr.scan(Id("akka-actors")){ v =>
      val versionString = v.attribute("version").values.head
      (versionString == "2.2.3")
    }.get
    
    res1 should be(true)
    

    //    val ivyResults1 = ivyHelper.ivyImport("com.typesafe.akka", "akka-actor_2.10", "2.2.1").right.value
    //    
    //    val repos1 = IvyHelper.insert(ivyResults1, baseDir)
    //    //...
    //    val ivyResults2 = ivyHelper.ivyImport("com.typesafe.akka", "akka-actor_2.10", "2.2.0").right.value
    //    val repos2 = IvyHelper.insert(ivyResults1, baseDir)
  }

  test("") {

  }
}