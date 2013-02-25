package adept.api

import org.scalatest._
import java.sql.Connection

class DescribeSpec extends FreshDBEachRun with ShouldMatchers {
  import db.driver.simple._
  import Helpers._
  import TestData._

  describe("Describing modules") {
    it("should work for nested dependencies") {
      val deps = modules(parent)
      deps.foreach { d =>
        Adept.add(repo, d, Seq.empty) should be(Right(d)) 
      }
      Adept.add(repo, parent, deps)
      val uberParent = Module(Coordinates("test", "uber","1.0"), Metadata(Map("test" -> "yes")), Hash("uberunique"))
      Adept.add(repo, uberParent , Seq(parent))
      println(Adept.describe(uberParent.coords, uberParent.metadata).right.map{ case (a,b) => a -> b.mkString("\n") } )
    }
  }
}
