package adept.repository

import org.scalatest.FunSpec
import org.scalatest.Matchers
import adept.resolution.models.Id
import adept.repository.models.VariantHash
import adept.repository.models.Commit
import org.scalatest.OptionValues._
import adept.repository.models.RepositoryName
import adept.repository.models.RankId
import adept.repository.metadata.RankingMetadata
import adept.test.FileUtils.usingTmpDir
import adept.test.HashUtils.asHash
import scala.Option.option2Iterable
import adept.repository.models.Ranking

class RankLogicTest extends FunSpec with Matchers {
  import adept.test.FileUtils.usingTmpDir
  import adept.test.HashUtils._

  describe("Variant hash pruning") {
    val id = Id("test/foo")

    val hash1: VariantHash = "foobar"
    val hash21: VariantHash = "foobar21"
    val hash22: VariantHash = "foobar22"
    val hash31: VariantHash = "foobar31"
    val hash32: VariantHash = "foobar32"
    val hash33: VariantHash = "foobar33"

    val rankId1 = RankId("foo1")
    val rankId2 = RankId("foo2")
    val rankId3 = RankId("foo3")
    val ranking1 = Ranking(id, rankId1, Seq())
    it("should check if one simple hash is found") {
      RankLogic.chosenVariants(Set(hash1), Set(
        Ranking(id, rankId1, Seq(hash1)))) shouldEqual Set(hash1)
    }
    it("should verify that hashes that are not known, are not pruned") {
      RankLogic.chosenVariants(Set(hash1, hash21), Set(
        Ranking(id, rankId1, Seq(hash1)))) shouldEqual Set(hash1, hash21)
    }
    it("should verify that 2 existing hashes behaves correctly (both are found)") {
      RankLogic.chosenVariants(Set(hash1, hash21), Set(
        Ranking(id, rankId1, Seq(hash1)),
        Ranking(id, rankId2, Seq(hash21)))) shouldEqual Set(hash1, hash21)
    }
    it("should verify that the old hashes are pruned away") {
      RankLogic.chosenVariants(Set(hash1, hash22, hash21), Set(
        Ranking(id, rankId1, Seq(hash1)),
        Ranking(id, rankId2, Seq(hash22, hash21)))) shouldEqual Set(hash1, hash22)
    }
    it("should verify that the latest ranking is included") {
      RankLogic.chosenVariants(Set(hash1, hash22, hash21), Set(
        Ranking(id, rankId1, Seq(hash1)),
        Ranking(id, rankId3, Seq(hash31)),
        Ranking(id, rankId2, Seq(hash22, hash21)))) shouldEqual Set(hash1, hash22, hash31)
    }
    it("should verify that the latest ranking always is the one which is defined (even if there is a newer one)") {
      RankLogic.chosenVariants(Set(hash1, hash22, hash21, hash31), Set(
        Ranking(id, rankId1, Seq(hash1)),
        Ranking(id, rankId2, Seq(hash32, hash31)),
        Ranking(id, rankId3, Seq(hash22, hash21)))) shouldEqual Set(hash1, hash22, hash31)
    }
    it("should verify that default active variants work as expected") {
      RankLogic.activeVariants(Set(
        Ranking(id, rankId1, Seq(hash1)),
        Ranking(id, rankId2, Seq(hash32, hash31)),
        Ranking(id, rankId3, Seq(hash22, hash21)))) shouldEqual Set(hash1, hash22, hash32)
    }
    it("should work with the multiple consecutive (same order but one has more) rankings with the same rankids") {
      RankLogic.chosenVariants(Set(hash1, hash22, hash21, hash31), Set(
        Ranking(id, rankId1, Seq(hash1)),
        Ranking(id, rankId2, Seq(hash32, hash31)), //<-- 32
        Ranking(id, rankId2, Seq(hash33, hash32, hash31)),
        Ranking(id, rankId3, Seq(hash22, hash21)))) shouldEqual Set(hash1, hash22, hash31)
    }
    it("should work with the multiple dissimilar (with a skipped) rankings with the same rankids") {
      RankLogic.chosenVariants(Set(hash1, hash22, hash21, hash32), Set(
        Ranking(id, rankId1, Seq(hash1)),
        Ranking(id, rankId2, Seq(hash33, hash31)), //<-- 33, skips 32
        Ranking(id, rankId2, Seq(hash33, hash32, hash31)),
        Ranking(id, rankId3, Seq(hash22, hash21)))) shouldEqual Set(hash1, hash22, hash32, hash33)
    }
    it("should work with the multiple dissimilar (with a new different hash) rankings with the same rankids") {
      RankLogic.chosenVariants(Set(hash1, hash22,  hash32), Set(
        Ranking(id, rankId1, Seq(hash1)),
        Ranking(id, rankId2, Seq(hash21, hash31)), //<-- 21 & 31
        Ranking(id, rankId2, Seq(hash33, hash32, hash31)),
        Ranking(id, rankId3, Seq(hash22, hash21)))) shouldEqual Set(hash1, hash21, hash22, hash32)
    }
  }

}