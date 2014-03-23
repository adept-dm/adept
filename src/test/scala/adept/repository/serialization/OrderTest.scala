package adept.repository.serialization

import org.scalatest.FunSuite
import org.scalatest.Matchers
import adept.resolution.models.Id
import adept.utils.Hasher
import adept.repository.models.VariantHash
import adept.repository.GitRepository
import adept.repository.models.Commit
import org.scalatest.OptionValues._
import adept.repository.models.RepositoryName

class OrderTest extends FunSuite with Matchers {
  import adept.test.FileUtils.usingTmpDir
  import adept.test.HashUtils._

//  test("Variant hash pruning") {
//    usingTmpDir { rootDir =>
//      val repository = new GitRepository(rootDir, RepositoryName("test-repo1"))
//      repository.init()
//      val id = Id("test/foo")
//
//      val hash1: VariantHash = "foobar"
//      val hash21: VariantHash = "foobar21"
//      val hash22: VariantHash = "foobar22"
//      val hash31: VariantHash = "foobar31"
//      val hash32: VariantHash = "foobar32"
//
//      repository.add(Order.insertNewFile(id, hash1, repository, repository.getHead))
//      repository.commit("New order (1)")
//      withClue("should check if one simple hash can be read") {
//        Order.chosenVariants(id, Set(hash1), repository, repository.getHead) shouldEqual Set(hash1)
//      }
//      withClue("should verify that hashes that are not known, are not pruned") {
//        Order.chosenVariants(id, Set(hash1, hash21), repository, repository.getHead) shouldEqual Set(hash1, hash21)
//      }
//      repository.add(Order.insertNewFile(id, hash21, repository, repository.getHead))
//      repository.commit("New order (2)")
//      withClue("should verify that 2 existing hashes behaves correctly (both are found)") {
//        Order.chosenVariants(id, Set(hash1, hash21), repository, repository.getHead) shouldEqual Set(hash1, hash21)
//      }
//      val orderId2 = Order.findOrderId(id, repository, repository.getHead) { hash =>
//        hash21 == hash
//      }.value
//      repository.add(Order.add(id, orderId2, hash22, repository, repository.getHead))
//      repository.commit("Updated order (2)")
//      withClue("should verify that the old hashes are pruned away") {
//        Order.chosenVariants(id, Set(hash1, hash22, hash21), repository, repository.getHead) shouldEqual Set(hash1, hash22)
//      }
//      repository.add(Order.insertNewFile(id, hash31, repository, repository.getHead))
//      repository.commit("New order (3)")
//      withClue("should verify that the latest ordering in a new file is included") {
//        Order.chosenVariants(id, Set(hash1, hash22, hash21), repository, repository.getHead) shouldEqual Set(hash1, hash22, hash31)
//      }
//      val orderId3 = Order.findOrderId(id, repository, repository.getHead) { hash =>
//        hash31 == hash
//      }.value
//      repository.add(Order.add(id, orderId3, hash32, repository, repository.getHead))
//      repository.commit("Updated order (3)")
//
//      withClue("should verify that the latest order always is the one which is defined (even if there is a newer one)") {
//        Order.chosenVariants(id, Set(hash1, hash22, hash21, hash31), repository, repository.getHead) shouldEqual Set(hash1, hash22, hash31)
//      }
//
//      withClue("verify that default active variants work as expected") {
//        Order.activeVariants(id, repository, repository.getHead) shouldEqual Set(hash1, hash22, hash32)
//      }
//    }
//  }
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