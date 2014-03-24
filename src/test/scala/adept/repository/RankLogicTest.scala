package adept.repository

import org.scalatest.FunSuite
import org.scalatest.Matchers
import adept.resolution.models.Id
import adept.repository.models.VariantHash
import adept.repository.models.Commit
import org.scalatest.OptionValues._
import adept.repository.models.RepositoryName
import adept.repository.models.RankId
import adept.repository.serialization.RankingMetadata
import adept.test.FileUtils.usingTmpDir
import adept.test.HashUtils.asHash
import scala.Option.option2Iterable

class RankLogicTest extends FunSuite with Matchers {
  import adept.test.FileUtils.usingTmpDir
  import adept.test.HashUtils._

  def newRanking(id: Id, hash: VariantHash, repository: GitRepository, commit: Commit) = {
    val current = RankingMetadata.listRankIds(id, repository, commit)
    val rankId = RankingMetadata.getXRankId(id, repository, current.size, 1).headOption.value
    RankingMetadata(Seq(hash)).write(id, rankId, repository)
  }

  def findRankings(id: Id, hash: VariantHash, repository: GitRepository, commit: Commit) = {
    val rankings = RankingMetadata.listRankIds(id, repository, commit).flatMap { rankId =>
      RankingMetadata.read(id, rankId, repository, commit).map(_.toRanking(id, rankId))
    }
    val matchingRankings = rankings.filter(_.variants.contains(hash))
    matchingRankings should have size (1)
    matchingRankings.headOption.value
  }

  def prepend(id: Id, rankId: RankId, hash: VariantHash, repository: GitRepository, commit: Commit) = {
    val rankingMetadata = RankingMetadata.read(id, rankId, repository, commit).headOption.value
    rankingMetadata.copy(variants = hash +: rankingMetadata.variants).write(id, rankId, repository)
  }

  val RankLogic = adept.repository.RankLogic.Default

  test("Variant hash pruning") {
    usingTmpDir { rootDir =>
      val repository = new GitRepository(rootDir, RepositoryName("test-repo1"))
      repository.init()
      val id = Id("test/foo")

      val hash1: VariantHash = "foobar"
      val hash21: VariantHash = "foobar21"
      val hash22: VariantHash = "foobar22"
      val hash31: VariantHash = "foobar31"
      val hash32: VariantHash = "foobar32"

      repository.add(newRanking(id, hash1, repository, repository.getHead))
      repository.commit("New ranking (1)")
      withClue("should check if one simple hash can be read") {
        RankLogic.getChosenVariants(id, Set(hash1), repository, repository.getHead) shouldEqual Set(hash1)
      }
      withClue("should verify that hashes that are not known, are not pruned") {
        RankLogic.getChosenVariants(id, Set(hash1, hash21), repository, repository.getHead) shouldEqual Set(hash1, hash21)
      }
      repository.add(newRanking(id, hash21, repository, repository.getHead))
      repository.commit("New ranking (2)")
      withClue("should verify that 2 existing hashes behaves correctly (both are found)") {
        RankLogic.getChosenVariants(id, Set(hash1, hash21), repository, repository.getHead) shouldEqual Set(hash1, hash21)
      }
      val rankId2 = findRankings(id, hash21, repository, repository.getHead).rankId
      repository.add(prepend(id, rankId2, hash22, repository, repository.getHead))
      repository.commit("Updated ranking (2)")
      withClue("should verify that the old hashes are pruned away") {
        RankLogic.getChosenVariants(id, Set(hash1, hash22, hash21), repository, repository.getHead) shouldEqual Set(hash1, hash22)
      }
      repository.add(newRanking(id, hash31, repository, repository.getHead))
      repository.commit("New ranking (3)")
      withClue("should verify that the latest ordering in a new file is included") {
        RankLogic.getChosenVariants(id, Set(hash1, hash22, hash21), repository, repository.getHead) shouldEqual Set(hash1, hash22, hash31)
      }
      val rankId3 = findRankings(id, hash31, repository, repository.getHead).rankId 
      repository.add(prepend(id, rankId3, hash32, repository, repository.getHead))
      repository.commit("Updated ranking (3)")

      withClue("should verify that the latest order always is the one which is defined (even if there is a newer one)") {
        RankLogic.getChosenVariants(id, Set(hash1, hash22, hash21, hash31), repository, repository.getHead) shouldEqual Set(hash1, hash22, hash31)
      }

      withClue("verify that default active variants work as expected") {
        RankLogic.getActiveVariants(id, repository, repository.getHead) shouldEqual Set(hash1, hash22, hash32)
      }
    }
  }
  //
  //  test("Order simple replace") {
  //    usingTmpDir { tmpDir =>
  //      val id = Id("A")
  //      val repository = new GitRepository(tmpDir, RepositoryName("test-repo1"))
  //      repository.init()
  //      val afterOldHash: VariantHash = "afteroldhash"
  //      val oldHash: VariantHash = "oldhash"
  //      val beforeOldHash: VariantHash = "beforeoldhash"
  //      repository.add(Order.insertNewFile(id, oldHash, repository, repository.getHead))
  //      repository.commit("Some order is required")
  //      repository.add(Order.listActiveOrderIds(id, repository, repository.getHead).flatMap { orderId =>
  //        Order.replace(id, orderId, repository, repository.getHead) { currentHash =>
  //          if (currentHash == oldHash) {
  //            Some(Seq(afterOldHash, oldHash, beforeOldHash))
  //          } else None
  //        }
  //      })
  //      repository.commit("Fixed order")
  //      Order.activeVariants(id, repository, repository.getHead) shouldEqual Set(afterOldHash)
  //    }
  //  }

  //  test("Updates of consecutive sets") {
  //    usingTmpDir { rootDir =>
  //      val repository = new GitRepository(rootDir, RepositoryName("test-repo1"))
  //      repository.init()
  //
  //      val id = Id("test/foo")
  //
  //      val firstHashes = Set("foo").map(s => VariantHash(Hasher.hash(s.getBytes)))
  //      val secondHashes = Set("loo", "zoo").map(s => VariantHash(Hasher.hash(s.getBytes)))
  //      val thirdHashes = Set("oops", "noos", "moo").map(s => VariantHash(Hasher.hash(s.getBytes)))
  //      val updatedHashes = secondHashes ++ Set("moo").map(s => VariantHash(Hasher.hash(s.getBytes)))
  //
  //      repository.add(
  //        Order.add(id, VariantSet(firstHashes), repository))
  //      repository.commit("Changed order")
  //
  //      repository.add(
  //        Order.add(id, VariantSet(secondHashes), repository))
  //      repository.commit("Changed order again")
  //
  //      repository.add(
  //        Order.add(id, VariantSet(thirdHashes), repository))
  //      repository.commit("Changed order AND again!")
  //
  //      repository.add(
  //        Order.add(id, VariantSet(Set("jeg", "er", "kul").map(s => VariantHash(Hasher.hash(s.getBytes)))), repository))
  //      repository.commit("Aaaand again!")
  //
  //      repository.add(
  //        Order.add(id, VariantSet(Set("du", "liker", "mat").map(s => VariantHash(Hasher.hash(s.getBytes)))), repository))
  //      repository.commit("Aaaand again (last one)!")
  //
  //      Order.update(id, secondHashes.slice(1, 2), VariantSet(updatedHashes), repository, Commit("HEAD"))
  //    }
  //  }

  //  test("Updates of last correct order") {
  //    usingTmpDir { rootDir =>
  //      val repository = new GitRepository(rootDir, RepositoryName("test-repo1"))
  //      repository.init()
  //
  //      val id = Id("test/foo")
  //
  //      val initial = Set("oops", "noos", "moo")
  //      val initialHashes = initial.map(s => VariantHash(Hasher.hash(s.getBytes)))
  //      val updatedHashes = (initial ++ Set("loo")).map(s => VariantHash(Hasher.hash(s.getBytes)))
  //
  //      repository.add(
  //        Order.add(id, VariantSet(initialHashes), repository))
  //      repository.commit("First order")
  //
  //      Order.update(id, initialHashes, VariantSet(updatedHashes), repository, Commit("HEAD"))
  //    }
  //  }
}