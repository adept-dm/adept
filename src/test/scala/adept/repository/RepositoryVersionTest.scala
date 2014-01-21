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
    def currentAkkaGitRepo = new LocalGitRepository(baseDir, "com.typesafe.akka", Commit.Head)

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

    val lgr205First = currentAkkaGitRepo.scan(Id("akka-actors")) { v =>
      val versionString = v.attribute("version").values.head
      (versionString == "2.0.5")
    }

    IvyHelper.insert(Set(IvyImportResult(ModuleRevisionId.newInstance("com.typesafe.akka", "akka-actors", "2.0.4"), variants = Set(
      Variant(Id("akka-actors"), attributes = Set(Attribute("version", Set("2.0.4"))), artifacts = Set.empty, dependencies = Set.empty)), artifacts = Set.empty, localFiles = Map.empty)),
      baseDir)

    IvyHelper.insert(Set(IvyImportResult(ModuleRevisionId.newInstance("com.typesafe.akka", "akka-actors", "2.2.3"), variants = Set(
      Variant(Id("akka-actors"), attributes = Set(Attribute("version", Set("2.2.3"))), artifacts = Set.empty, dependencies = Set.empty)), artifacts = Set.empty, localFiles = Map.empty)),
      baseDir)

    IvyHelper.insert(Set(IvyImportResult(ModuleRevisionId.newInstance("com.typesafe.akka", "akka-actors", "2.2.2"), variants = Set(
      Variant(Id("akka-actors"), attributes = Set(Attribute("version", Set("2.2.2"))), artifacts = Set.empty, dependencies = Set.empty)), artifacts = Set.empty, localFiles = Map.empty)),
      baseDir)

      
    val res1 = currentAkkaGitRepo.scan(Id("akka-actors")) { v =>
      val versionString = v.attribute("version").values.head
      (versionString == "2.0.5")
    }.get < currentAkkaGitRepo.scan(Id("akka-actors")) { v =>
      val versionString = v.attribute("version").values.head
      (versionString == "2.2.3")
    }.get

    res1 should be(true)

    val res2 = lgr205First.get < currentAkkaGitRepo.scan(Id("akka-actors")) { v =>
      val versionString = v.attribute("version").values.head
      (versionString == "2.2.3")
    }.get

    res2 should be(true)

    //    val ivyResults1 = ivyHelper.ivyImport("com.typesafe.akka", "akka-actor_2.10", "2.2.1").right.value
    //    
    //    val repos1 = IvyHelper.insert(ivyResults1, baseDir)
    //    //...
    //    val ivyResults2 = ivyHelper.ivyImport("com.typesafe.akka", "akka-actor_2.10", "2.2.0").right.value
    //    val repos2 = IvyHelper.insert(ivyResults1, baseDir)
  }

  test("modificiations compression") {

    val allLines = Map(
      ("00a9e88e1aefa17558f5cd508716e8b36bdfcb1f", "b076dbb384269750592073ca7e9a306c00764aad"),
      ("34df8502473f106e19265c91b3a12bb8f4f2f483", "f3e7fa9a053d99f5fa7801b26d7207707ef76ef2"),
      ("2deeb6d3029d447afc22e995ac13658019e1b965", "e898291fa8d54834b4a991b48d54db332e2e9d95"),
      ("e898291fa8d54834b4a991b48d54db332e2e9d95", "bc76191f55adee86654029d9ef3ebe75778619e6"),
      ("bc76191f55adee86654029d9ef3ebe75778619e6", "foo"),
      ("c9959dc35f88d2adf765568d6af2abee08b59d10", "3d93042d4b8da7e6a2049bd2809224544b48f9d0"),
      ("3f67d686c3f24b199f3a3610b5129041c4f01233", "a7c4e10625f92b18d4011fc652f017264ed28454"),
      ("3d93042d4b8da7e6a2049bd2809224544b48f9d0", "bar"))

    val result: Set[Set[String]] = Set(
      Set("00a9e88e1aefa17558f5cd508716e8b36bdfcb1f", "b076dbb384269750592073ca7e9a306c00764aad"),
      Set("34df8502473f106e19265c91b3a12bb8f4f2f483", "f3e7fa9a053d99f5fa7801b26d7207707ef76ef2"),
      Set("2deeb6d3029d447afc22e995ac13658019e1b965", "e898291fa8d54834b4a991b48d54db332e2e9d95", "bc76191f55adee86654029d9ef3ebe75778619e6", "foo"),
      Set("c9959dc35f88d2adf765568d6af2abee08b59d10", "3d93042d4b8da7e6a2049bd2809224544b48f9d0", "bar"),
      Set("3f67d686c3f24b199f3a3610b5129041c4f01233", "a7c4e10625f92b18d4011fc652f017264ed28454"))

    val compressedStrings = LocalGitRepository.compressModifications(allLines)
    result should be(compressedStrings)
  }
}