package adept.core

import org.scalatest._
import java.sql.Connection

class DBSpec extends FreshDBEachRun with ShouldMatchers {
  import db.driver.simple._
  import Helpers._
  import TestData._

  def allModules = database.withSession{
    import slick.session.Database.threadLocalSession
    
    val foundModules = (for{
      m <- Modules
    } yield {
      m
    }).list.map{ t => 
      val (m, r) = Modules.fromRow(t)
      (m, r, t._11)//_11 is deleted fix this!
    }
    foundModules
  }

  def expectRight[A, B](res:Either[A, B], reason: String) = {
    assert(res.isRight, "Expected Right, but got: "+res + ". Expected a Right because: "+reason)
  }
  def expectLeft[A, B](res:Either[A, B], reason: String) = {
    assert(res.isLeft, "Expected Left, but got: "+res + ". Expected a Left because: "+reason)
  }
  
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
      expectRight(Adept.remove(repoName, parent.hash), "removing should be fine")
      val commitedResAfterFix = Adept.commit(repoName)
      expectRight(commitedResAfterFix, "we just removed the module depending on the child deleted earlier")
    }
  }
  
  describe("Setting modules") {
    it("should fix if missing deps are added after an add") {
      expectRight(Adept.add(repoName, parent.copy(deps = modules(parent).map(_.hash))), "adding should work")
      expectRight(Adept.set(repoName, parent), "setting is always ok")
      expectRight(Adept.commit(repoName), "all requirements are fulfilled for committing")
      
      
      expectRight(Adept.set(repoName, parent.copy(deps = modules(parent).map(_.hash))), "updating an existing module should work")
      val commitedRes = Adept.commit(repoName)
      expectLeft(commitedRes, "we just added a missing child dependency")
      commitedRes.left.foreach(_ should include(modules(parent).toSeq(0).hash.toString))
      commitedRes.left.foreach(_ should include(modules(parent).toSeq(1).hash.toString))
      
      expectRight(Adept.add(repoName, modules(parent).toSeq(0)), "adding should work") 
      expectRight(Adept.add(repoName, modules(parent).toSeq(1)), "adding should work") 
      expectRight(Adept.commit(repoName), "all requirements are fulfilled for committing")
    }
  }

  describe("Doing the same operation multiple times") {
    it("should be end up in the correct state"){
      val parentWithDeps = parent.copy(deps = modules(parent).map(_.hash))
      
      Adept.add(repoName, parentWithDeps)
      Adept.set(repoName, parent)
      Adept.commit(repoName)
      val commit0Modules = Set((parent, Repository(repoName, 0), false))
      allModules.toSet should equal(commit0Modules)
      Adept.list(repoName).right.get.toSet should equal(commit0Modules.map{case (m,r,d) => m -> r})
      
      Adept.set(repoName, parentWithDeps)
      expectLeft(Adept.commit(repoName), "we just added a missing child dependency")
      allModules.toSet should equal(commit0Modules ++ Set((parentWithDeps, Repository(repoName, 1), false)))
      Adept.list(repoName).right.get.toSet should equal(Set(
        (parent, Repository(repoName, 0))
      ))
      
      Adept.add(repoName, modules(parent).toSeq(0))
      Adept.add(repoName, modules(parent).toSeq(1))
      val commit1Modules = Set(
        (parentWithDeps, Repository(repoName, 1), false),
        (modules(parent).toSeq(0), Repository(repoName, 1), false),
        (modules(parent).toSeq(1), Repository(repoName, 1), false)
      )
      Adept.commit(repoName)
      allModules.toSet should equal(commit0Modules ++ commit1Modules)
      Adept.list(repoName).right.get.toSet should equal(Set(
        (parentWithDeps, Repository(repoName, 1)),
        (modules(parent).toSeq(0), Repository(repoName, 1)),
        (modules(parent).toSeq(1), Repository(repoName, 1))    
      ))
      
      //multiple repetition
      Adept.remove(repoName, parent.hash)
      Adept.remove(repoName, parent.hash)
      Adept.remove(repoName, parent.hash)
      Adept.add(repoName, parent)
      Adept.remove(repoName, parent.hash)
      Adept.remove(repoName, parent.hash)
      Adept.add(repoName, parent)
      Adept.add(repoName, parent)
      Adept.add(repoName, parent)
      Adept.add(repoName, parent)
      Adept.set(repoName, parent)
      Adept.set(repoName, parent)
      Adept.set(repoName, parent)
      Adept.commit(repoName)
      val commit2Modules = Set((parent, Repository(repoName, 2), false))
      allModules.toSet should equal(commit0Modules ++ commit1Modules ++ commit2Modules)
      Adept.list(repoName).right.get.toSet should equal(Set(
        (parent, Repository(repoName, 2)),
        (modules(parent).toSeq(0), Repository(repoName, 1)),
        (modules(parent).toSeq(1), Repository(repoName, 1))    
      ))
      
      
      Adept.remove(repoName, parent.hash)
      Adept.commit(repoName)
      val commit3Modules = Set((parent, Repository(repoName, 3), true))
      allModules.toSet should equal(commit0Modules ++ commit1Modules ++ commit2Modules ++ commit3Modules)
      
      Adept.list(repoName).right.get.toSet should equal(Set(
        (modules(parent).toSeq(0), Repository(repoName, 1)),
        (modules(parent).toSeq(1), Repository(repoName, 1))    
      ))
      
      Adept.add(repoName, parent)
      Adept.commit(repoName)
            
      val commit4Modules = Set((parent, Repository(repoName, 4), false))
      allModules.toSet should equal(commit0Modules ++ commit1Modules ++ commit2Modules ++ commit3Modules ++ commit4Modules)
      
      Adept.list(repoName).right.get.toSet should equal(Set(
        (parent, Repository(repoName, 4)),
        (modules(parent).toSeq(0), Repository(repoName, 1)),
        (modules(parent).toSeq(1), Repository(repoName, 1))    
      ))
      
    }
    
    it("should be give the correct message"){
      val parentWithDeps = parent.copy(deps = modules(parent).map(_.hash))
      expectRight(Adept.add(repoName, parentWithDeps), "adding should work")
      expectRight(Adept.set(repoName, parent), "updating are ok now")
      expectRight(Adept.commit(repoName), "all requirements are fulfilled for committing")
      
      expectRight(Adept.set(repoName, parentWithDeps), "updating an existing module should work")
      val commitedRes = Adept.commit(repoName)
      
      expectLeft(commitedRes, "we just added a missing child dependency")
      commitedRes.left.foreach(_ should include(modules(parent).toSeq(0).hash.toString))
      commitedRes.left.foreach(_ should include(modules(parent).toSeq(1).hash.toString))
      
      expectRight(Adept.add(repoName, modules(parent).toSeq(0)), "adding should work") 
      expectRight(Adept.add(repoName, modules(parent).toSeq(1)), "adding should work") 
      expectRight(Adept.commit(repoName), "all requirements are fulfilled for committing")
      
      expectRight(Adept.remove(repoName, parent.hash), "remove a known should work")
      expectLeft(Adept.remove(repoName, parent.hash), "removing something again should not work")
      expectLeft(Adept.remove(repoName, parent.hash), "removing two times in a row produces consistent results")
      
      expectRight(Adept.add(repoName, parent), "adding is ok, because it was just removed")
      expectRight(Adept.remove(repoName, parent.hash), "removing is ok, because it was added")
      expectLeft(Adept.remove(repoName, parent.hash), "removing something again should not work")

      expectRight(Adept.set(repoName, parent), "set should always work") 
      expectRight(Adept.set(repoName, parent), "set should always work") 
      expectRight(Adept.set(repoName, parent), "set should always work") 
      
      expectRight(Adept.add(repoName, parent), "adding after removing should work")
      expectRight(Adept.add(repoName, parent), "adding again should give the same response as before")
      expectRight(Adept.commit(repoName), "we have some changes which should work")
      
      expectRight(Adept.remove(repoName, parent.hash), "should work since it was just added")

      expectRight(Adept.commit(repoName), "we have some changes and committing should work")
      expectRight(Adept.add(repoName, parent), "adding again should also work")
      
      expectRight(Adept.set(repoName, parent), "set should always work")
    }
  }
}
