package adept.core

import org.scalatest._
import java.sql.Connection
import util._
import adept.core.db.Types._

import adept.core.models._
import adept.core.db._
import adept.core.db.DAO.driver.simple._
  
class DBSpec extends FreshDBEachRun with ShouldMatchers {
  import TestData._
  
  def modules(adept: TestAdept): (List[ModuleRowType], List[ModuleRowType]) = {
    adept.stagedDBpublic.withSession{ implicit s: Session =>
      Query(Modules).list.map(Modules.fromRow)
    } -> adept.mainDBpublic.withSession{ implicit s: Session =>
      Query(Modules).list.map(Modules.fromRow)
    }
  }
  
  val firstCommitHash = "8b0e6a8e1d4f6a357bf343a23e93abc8b1e7213e"

  test("set should write to staged") {
    adept.set(parent)
    val (stagedRes, _) = modules(adept) 
    stagedRes should be === Seq((parent, None, false))
  }

  test("committing should move from staged to main") {
    adept.set(parent)
    adept.commit()
    val (stagedRes, mainRes) = modules(adept) 
    stagedRes should have size(0)
    mainRes should be === Seq((parent, Some(firstCommitHash), false))
  }
  
  test("set, commit, delete") {
    adept.set(parent)
    adept.commit()
    adept.delete(parent.hash)
    val (stagedRes, mainRes) = modules(adept) 
    stagedRes should be === Seq((parent, None, true))
    mainRes should be === Seq((parent, Some(firstCommitHash), false))
  }
  
  test("set, commit, delete, commit") {
    adept.set(parent)
    adept.commit()
    adept.delete(parent.hash)
    adept.commit()
    val (stagedRes, mainRes) = modules(adept) 
    stagedRes should have size(0)
    mainRes should be === Seq((parent, Some(firstCommitHash), false), (parent, Some("ac088075ad783b3d071ee553cb2f6b70b294c301"), true))
  }
  
  test("multiple set pre-commit") {
    adept.set(parent)
    adept.set(parent)
    adept.set(parent)
    val (stagedRes, _) = modules(adept)
    stagedRes should have size(1)
  }
  
  test("multiple delete pre-commit") {
    adept.delete(parent.hash)
    adept.delete(parent.hash)
    adept.delete(parent.hash)
    val (stagedRes, _) = modules(adept)
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
    modules(adept) match { case(stagedRes, mainRes) =>
      stagedRes should have size(0)
      mainRes should have size(1)
    }
  }

  test("multiple commit") {
    adept.set(parent)
    adept.commit()
    adept.delete(parent.hash)
    adept.commit()
    modules(adept) match { case(stagedRes, mainRes) =>
      stagedRes should have size(0)
      mainRes should have size(2)
    }
    
    adept.commit()
    adept.commit()
    adept.commit()
    modules(adept) match { case(stagedRes, mainRes) =>
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
    modules(adept) match { case(stagedRes, mainRes) =>
      stagedRes should have size(1)
      mainRes should have size(1)
    }
    
    adept.commit()
    modules(adept) match { case(stagedRes, mainRes) =>
      stagedRes should have size(0)
      mainRes should have size(2)
    }
    
    adept.delete(parent.hash)
    adept.delete(parent.hash)
    adept.delete(parent.hash)
    modules(adept) match { case(stagedRes, mainRes) =>
      stagedRes should have size(0)
      mainRes should have size(2)
    }
  }
}
