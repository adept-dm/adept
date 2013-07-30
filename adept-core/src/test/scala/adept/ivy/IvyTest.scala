package adept.ivy

import org.scalatest._
import adept.core.operations._
import adept.core.models._
import org.apache.ivy.core.IvyContext
import java.io.File
import adept.core.Adept
import org.eclipse.jgit.util.FileUtils

class IvyTest extends FunSuite with MustMatchers {

  test("ivy basic") {
    import org.scalatest.EitherValues._
    val ivy = IvyContext.getContext.getIvy //TODO: is this right?

    ivy.configure(new File("adept-core/src/test/resources/typesafe-ivy-settings.xml"))
    val adeptDir = new File("tests/localrepo")
    try {
      
    val testRepo = "local"
    val adept = Adept.open(adeptDir, testRepo) match {
      case Left(_) => Adept.init(adeptDir, testRepo) match {
        case Right(adept) => adept
      }
      case Right(adept) => adept
    }
    val coords = Coordinates("com.typesafe.akka", "akka-actor_2.10", "2.1.2")
    IvyImport.add(coords, ivy, adept) must have size(3)
    
    adept.findModule(coords, None, Set.empty).right.value
    } finally {
      FileUtils.delete(adeptDir, FileUtils.RECURSIVE)
    }
  }
}