package adept.repository.serialization

import org.scalatest.FunSuite
import org.scalatest.matchers.MustMatchers
import adept.resolution.models.Id
import adept.utils.Hasher
import adept.repository.models.VariantHash
import adept.repository.models.VariantSet
import adept.repository.GitRepository
import adept.repository.models.Commit
import org.scalatest.OptionValues._
import adept.repository.models.RepositoryName

class OrderTest extends FunSuite with MustMatchers {
  import adept.test.FileUtils.usingTmpDir

  test("Create and read single line in new File") {
    usingTmpDir { rootDir =>
      val repository = new GitRepository(rootDir, RepositoryName("test-repo1"))
      repository.init()
      val id = Id("test/foo")

      val hashes = Set("foo", "bar", "too").map(s => VariantHash(Hasher.hash(s.getBytes)))

      repository.add(Order.add(id, VariantSet(hashes), repository))
      repository.commit("Changed order")

      Order.firstMatch(id, hashes, repository, repository.getHead).value must be === VariantSet(hashes)
    }
  }

  test("Multiple adds and reads") {
    usingTmpDir { rootDir =>
      val repository = new GitRepository(rootDir, RepositoryName("test-repo1"))
      repository.init()

      val id = Id("test/foo")

      val firstHashes = Set("foo").map(s => VariantHash(Hasher.hash(s.getBytes)))
      val secondHashes = Set("loo", "zoo").map(s => VariantHash(Hasher.hash(s.getBytes)))
      val thirdHashes = Set("oops", "noos", "moo").map(s => VariantHash(Hasher.hash(s.getBytes)))

      repository.add(
        Order.add(id, VariantSet(firstHashes), repository))
      repository.commit("Changed order")

      repository.add(
        Order.add(id, VariantSet(secondHashes), repository))
      repository.commit("Changed order again")

      repository.add(
        Order.add(id, VariantSet(thirdHashes), repository))
      repository.commit("Changed order AGAIN!")

      Order.firstMatch(id, secondHashes.slice(0, 1), repository, Commit("HEAD~2")) must be === None
      Order.firstMatch(id, secondHashes.slice(0, 1), repository, Commit("HEAD~1")).value must be === VariantSet(secondHashes)
      Order.firstMatch(id, secondHashes.slice(0, 1), repository, Commit("HEAD")).value must be === VariantSet(secondHashes)
    }
  }
//
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