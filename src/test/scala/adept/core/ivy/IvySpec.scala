package adept.core.ivy
import org.scalatest._
import adept.core.models._
import org.apache.ivy.core.module.id.ModuleRevisionId
import adept.core.FreshDBEachRun
import adept.core.TestData
import adept.core.Configuration

class IvySpec extends FreshDBEachRun with ShouldMatchers {
  import TestData._
  
  test("ivy") {
    
      val ivy = IvyHelpers.load().right.get
      val coords = Coordinates("commons-cli", "commons-cli", "1.0")
      
      val res = IvyHelpers.set(coords, ivy, Configuration.defaultIvyConf, adept)
      println(res)
      throw new Exception("not completed")
      //res should be(Right(Seq(expectedModule)))
      
          
  }
}