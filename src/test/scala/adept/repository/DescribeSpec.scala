package adept.repository

import org.scalatest._
import adept.repository._
import java.sql.Connection

class DescribeSpec extends FreshDBEachRun with ShouldMatchers {
  import db.driver.simple._
  import Helpers._
  import TestData._

  describe("Describing descriptors") {
    it("should work for nested dependencies") {
      val deps = descriptors(parent)
      deps.foreach { d =>
        Repository.add(repo, d, Seq.empty) should be(Right(d)) 
      }
      Repository.add(repo, parent, deps)
      val uberParent = Descriptor(Coordinates("test", "uber","1.0"), Metadata(Map("test" -> "yes")), Hash("uberunique"))
      Repository.add(repo, uberParent , Seq(parent))
      println(Repository.describe(uberParent.coords, uberParent.metadata).right.map{ case (a,b) => a -> b.mkString("\n") } )
    }
  }
}