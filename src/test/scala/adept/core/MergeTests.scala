package adept.core

import org.scalatest._
import adept.core.models._
import adept.core.db._
import adept.core.db.DAO.driver.simple._
import adept.core.remote._
import java.io.File
import adept.core.operations.Merge

class MergeTests extends FreshDBEachRun with ShouldMatchers {

  import TestData._
  import Helpers._

  test("basic fast foward merge") {
    adept.set(parent)
    adept.commit.get
    adept.delete(parent.hash)
    adept.set(modules(parent).toSeq(0))
    adept.commit
    val adept2Dir = createTmpFolder().getRoot
    val checkpoint = Adept.clone(adept2Dir, adept.dir, repoName).get
    val adept2 = Adept(adept2Dir, repoName)
    
    
    adept.set(modules(parent).toSeq(1))
    adept.commit()
    adept2.set(modules(parent).toSeq(2))
    val adept2Commit = adept2.commit().get
    val changeSets = Merge.findChanges(checkpoint, adept.mainDB)
    Merge.fastForward(checkpoint, changeSets.get, adept2.mainDB)

    val committedModulesIn2 = Helpers.listChanges(adept2)._2
    val committedModulesIn1 = Helpers.listChanges(adept)._2
    committedModulesIn2.diff(committedModulesIn1) should be === Seq((modules(parent).toSeq(2), Some(adept2Commit.value), false))
  }
}