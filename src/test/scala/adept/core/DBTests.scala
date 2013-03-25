package adept.core

import org.scalatest._
import util._
import adept.core.db.Types._

import adept.core.models._
import adept.core.db._
import adept.core.db.DAO.driver.simple._
  
class DBTests extends FreshDBEachRun with ShouldMatchers {
  import TestData._
  import Helpers._

  val firstCommitHash = "58b9f19bc667dd5049d916486c280195405a1fee"

  test("set should write to staged") {
    adept.set(parent)
    val (stagedRes, _) = listChanges(adept) 
    stagedRes should be === Seq((parent, None, false))
  }

  test("committing should move from staged to main") {
    adept.set(parent)
    adept.commit()
    val (stagedRes, mainRes) = listChanges(adept) 
    stagedRes should have size(0)
    mainRes should be === Seq((parent, Some(firstCommitHash), false))
  }
  
  test("set, commit, delete") {
    adept.set(parent)
    adept.commit()
    adept.delete(parent.hash)
    val (stagedRes, mainRes) = listChanges(adept) 
    stagedRes should be === Seq((parent, None, true))
    mainRes should be === Seq((parent, Some(firstCommitHash), false))
  }
  
  test("set, commit, delete, commit") {
    adept.set(parent)
    adept.commit()
    adept.delete(parent.hash)
    adept.commit()
    val (stagedRes, mainRes) = listChanges(adept) 
    stagedRes should have size(0)
    mainRes should be === Seq((parent, Some(firstCommitHash), false), (parent, Some("4f2319b35ac37ca6731b336e17f55d22311ef395"), true))
  }
  
  test("multiple set pre-commit") {
    adept.set(parent)
    adept.set(parent)
    adept.set(parent)
    val (stagedRes, _) = listChanges(adept)
    stagedRes should have size(1)
  }
  
  test("multiple delete pre-commit") {
    adept.delete(parent.hash)
    adept.delete(parent.hash)
    adept.delete(parent.hash)
    val (stagedRes, _) = listChanges(adept)
    stagedRes should have size(0)
  }
  
  test("multiple set post-commit") {
    adept.set(parent)
    adept.set(parent)
    adept.set(parent)
    adept.commit()
    adept.set(parent)
    adept.set(parent)
    adept.set(parent)
    listChanges(adept) match { case(stagedRes, mainRes) =>
      stagedRes should have size(0)
      mainRes should have size(1)
    }
  }

  test("multiple commit") {
    adept.set(parent)
    adept.commit()
    adept.delete(parent.hash)
    adept.commit()
    listChanges(adept) match { case(stagedRes, mainRes) =>
      stagedRes should have size(0)
      mainRes should have size(2)
    }
    
    adept.commit()
    adept.commit()
    adept.commit()
    listChanges(adept) match { case(stagedRes, mainRes) =>
      stagedRes should have size(0)
      mainRes should have size(2)
    }
  }
  
  test("multiple delete post-commit") {
    adept.set(parent)
    adept.commit()
    adept.delete(parent.hash)
    adept.delete(parent.hash)
    adept.delete(parent.hash)
    listChanges(adept) match { case(stagedRes, mainRes) =>
      stagedRes should have size(1)
      mainRes should have size(1)
    }
    
    adept.commit()
    listChanges(adept) match { case(stagedRes, mainRes) =>
      stagedRes should have size(0)
      mainRes should have size(2)
    }
    
    adept.delete(parent.hash)
    adept.delete(parent.hash)
    adept.delete(parent.hash)
    listChanges(adept) match { case(stagedRes, mainRes) =>
      stagedRes should have size(0)
      mainRes should have size(2)
    }
  }
}
