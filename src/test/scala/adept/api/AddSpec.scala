package adept.api

import org.scalatest._
import java.sql.Connection

class AddSpec extends FreshDBEachRun with ShouldMatchers {
  import db.driver.simple._
  import Helpers._
  import TestData._

  describe("Adding modules") {
    it("should be idempotent") {
      Adept.add(repo, parent, Seq.empty) should be(Right(parent))
      Adept.add(repo, parent, Seq.empty) should be(Right(parent))
    }
    it("should fail if different metadata") {
      Adept.add(repo, parent, Seq.empty) should be(Right(parent))
      Adept.add(repo, parent.copy(metadata = Metadata(Map("different"->"stuff"))), Seq.empty).isLeft should be(true)
    }
    it("should fail if dependencies are not already inserted") {
      Adept.add(repo, parent, modules(parent)).isLeft should be(true) 
    }
    it("should work with dependencies") {
      val deps = modules(parent)
      deps.foreach { d =>
        Adept.add(repo, d, Seq.empty) should be(Right(d)) 
      }
      Adept.add(repo, parent, deps) should be(Right(parent))
    }
  }
}
