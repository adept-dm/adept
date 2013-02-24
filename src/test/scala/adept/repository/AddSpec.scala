package adept.repository

import org.scalatest._
import adept.repository._
import java.sql.Connection

class AddSpec extends FreshDBEachRun with ShouldMatchers {
  import db.driver.simple._
  import Helpers._
  import TestData._

  describe("Adding descriptors") {
    it("should be idempotent") {
      Repository.add(repo, parent, Seq.empty) should be(Right(parent))
      Repository.add(repo, parent, Seq.empty) should be(Right(parent))
    }
    it("should fail if different metadata") {
      Repository.add(repo, parent, Seq.empty) should be(Right(parent))
      Repository.add(repo, parent.copy(metadata = Metadata(Map("different"->"stuff"))), Seq.empty).isLeft should be(true)
    }
    it("should fail if dependencies are not already inserted") {
      Repository.add(repo, parent, descriptors(parent)).isLeft should be(true) 
    }
    it("should work with dependencies") {
      val deps = descriptors(parent)
      deps.foreach { d =>
        Repository.add(repo, d, Seq.empty) should be(Right(d)) 
      }
      Repository.add(repo, parent, deps) should be(Right(parent))
    }
  }
}