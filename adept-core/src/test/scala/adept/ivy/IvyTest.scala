package adept.ivy

import org.scalatest._
import adept.core.operations._
import adept.core.models._
import org.apache.ivy.core.IvyContext
import java.io.File
import adept.core.Adept

class IvyTest extends FunSuite with MustMatchers {

  test("ivy basic") {
    val ivy = IvyContext.getContext.getIvy //TODO: is this right?

    ivy.configure(new File("../typesafe-ivy-settings.xml"))
    val adeptDir = new File("/Users/freekh/.adept")
    val testRepo = "local"
    val adept = Adept.open(adeptDir, testRepo) match {
      case Left(_) => Adept.init(adeptDir, testRepo) match {
        case Right(adept) => adept
      }
      case Right(adept) => adept
    }
    IvyImport.add(Coordinates("play", "play_2.10", "2.1.2"), ivy, adept)
  }
}