package adept.core

import org.scalatest._
import java.sql.Connection

class DBSpec extends FreshDBEachRun with ShouldMatchers {
  import db.driver.simple._
  import Helpers._
  import TestData._

  def printTest = database.withSession{  //TODO: remove
    import slick.session.Database.threadLocalSession
    println("___________")
    println((for{
      m <- Modules
    } yield {
      m
    }).list.mkString("\n\t", "\n\t", ""))
    println("^^^^^^^^^^^^^")

  }

  def expectRight[A, B](res:Either[A, B], reason: String) = {
    assert(res.isRight, "Expected Right, but got: "+res + ". Expected a Right because: "+reason)
  }
  def expectLeft[A, B](res:Either[A, B], reason: String) = {
    assert(res.isLeft, "Expected Left, but got: "+res + ". Expected a Left because: "+reason)
  }
  /*
  describe("Adding modules") {
    it("should be work correctly when committing") {
      expectRight(Adept.add(repoName, parent), "adding should work")
      expectRight(Adept.commit(repoName), "commiting after a successful add without anything missing should work")
      expectLeft(Adept.add(repoName, parent.copy(metadata = Metadata(Map.empty))), "changing meta should not be possible")
      expectLeft(Adept.add(repoName, parent.copy(deps = Set(Hash("foo")))), "changing deps should not be possible")
      expectLeft(Adept.add(repoName, parent.copy(artifactHash = Hash("woow"))), "changing artifact hash should not be possible")
      expectLeft(Adept.commit(repoName), "having nothing to commit because the ones above failed")
    }
  }
  describe("Commiting modules") {
    it("should only be possible if dependencies are already there") {
      expectRight(Adept.add(repoName, parent.copy(deps = modules(parent).map(_.hash))), "no reason to fail")
      val commitedRes = Adept.commit(repoName)
      expectLeft(commitedRes, "there are missing deps in the line above")
      commitedRes.left.foreach(_ should include(modules(parent).toSeq(0).hash.toString))
      commitedRes.left.foreach(_ should include(modules(parent).toSeq(1).hash.toString))
      
      modules(parent).foreach{ depModule =>
        expectRight(Adept.add(repoName, depModule), "there is nothing special")
      }
      expectRight(Adept.commit(repoName), "commiting should now work because all deps are inserted")
    }
  }
  
  describe("Removing modules") {
    it("should fail on commit if a dependency is deleted BEFORE being committed") {
      expectRight(Adept.add(repoName, parent.copy(deps = modules(parent).map(_.hash))), "adding should work") 
      expectRight(Adept.add(repoName, modules(parent).toSeq(0)), "adding should work") 
      expectRight(Adept.add(repoName, modules(parent).toSeq(1)), "adding should work") 
      expectRight(Adept.remove(repoName, modules(parent).toSeq(1).hash), "we just removed a a dependency")
      expectLeft(Adept.commit(repoName), "we just removed a a dependency")
    }
    
    it("should fail on commit if a depedency is deleted AFTER it has been committed") {
      expectRight(Adept.add(repoName, parent.copy(deps = modules(parent).map(_.hash))), "adding should work") 
      expectRight(Adept.add(repoName, modules(parent).toSeq(0)), "adding should work") 
      expectRight(Adept.add(repoName, modules(parent).toSeq(1)), "adding should work") 
      expectRight(Adept.commit(repoName), "all requirements are fulfilled for committing")
      expectRight(Adept.remove(repoName, modules(parent).toSeq(1).hash), "removing should be fine")
      val commitedRes = Adept.commit(repoName)
      expectLeft(commitedRes, "we just removed a child dependency")
      commitedRes.left.foreach(_ should include(modules(parent).toSeq(1).hash.toString))
    }
  }
  */
  describe("Updating modules") {
    it("should fail when updating with missing deps and work if they are added") {
      expectRight(Adept.add(repoName, parent.copy(deps = modules(parent).map(_.hash))), "adding should work")
      expectRight(Adept.update(repoName, parent.hash, parent), "updating are ok now")
      println("--"+Adept.commit(repoName), "all requirements are fulfilled for committing")
      
      
      println(Adept.update(repoName, parent.hash, parent.copy(deps = modules(parent).map(_.hash))), "updating an existing module should work")
      val commitedRes = Adept.commit(repoName)
      expectLeft(commitedRes, "we just added a missing child dependency")
      commitedRes.left.foreach(_ should include(modules(parent).toSeq(0).hash.toString))
      commitedRes.left.foreach(_ should include(modules(parent).toSeq(1).hash.toString))
      
      expectRight(Adept.add(repoName, modules(parent).toSeq(0)), "adding should work") 
      expectRight(Adept.add(repoName, modules(parent).toSeq(1)), "adding should work") 
      expectRight(Adept.commit(repoName), "all requirements are fulfilled for committing")
      
      //printTest
      println("_________=======")
      println("add"+Adept.add(repoName, parent)) //adding the same as we deleted in active, so we unmark it as deleted
      
      println(Adept.remove(repoName, parent.hash))
      println(Adept.remove(repoName, parent.hash))
      //printTest
      println("up"+Adept.update(repoName, parent.hash, parent)) //not possible 
      println("up"+Adept.update(repoName, parent.hash, parent)) //not possible 
      println("add"+Adept.add(repoName, parent)) //adding the same as we deleted in active, so we unmark it as deleted
      println("add"+Adept.add(repoName, parent)) //adding the same as we deleted in active, so we unmark it as deleted
      Adept.commit(repoName)

      println(Adept.remove(repoName, parent.hash))

      println(Adept.commit(repoName))
      println("_!))___"+ Adept.add(repoName, parent))
      
      println("=====>" + Adept.update(repoName, parent.hash, parent))
      
      printTest
      
      Adept.diff(repoName)
    }
  }
}
