package adept.repository

import org.scalatest.FunSuite
import org.scalatest.matchers.MustMatchers
import org.eclipse.jgit.lib.TextProgressMonitor
import adept.repository.serialization.VariantMetadata
import adept.repository.models._
import adept.resolution.models._
import net.sf.ehcache.CacheManager
import adept.repository.serialization.Order
import java.io.File
import adept.ext.Version
import org.scalatest.OptionValues._
import adept.ext.VersionOrder
import adept.repository.serialization.RepositoryMetadata
import adept.ext.AttributeDefaults

class RepositoryTest extends FunSuite with MustMatchers {
  import adept.test.FileUtils._
  import adept.test.ResolverUtils._

  def basicSetup(dir: File) = {
    val progress = new TextProgressMonitor()
    val repoA = new GitRepository(dir, RepositoryName("com.a"), progress)
    repoA.init()
    val repoB = new GitRepository(dir, RepositoryName("com.b"), progress)
    repoB.init()
    val info = Set(
      Variant("A", Set(version -> Set("1.0.0"), binaryVersion -> Set("1.0")),
        requirements = Set("B" -> Set(Constraint(binaryVersion, Set("2.0"))))) -> repoA,

      Variant("B", Set(version -> Set("2.0.1"), binaryVersion -> Set("2.0")),
        requirements = Set.empty) -> repoB)

    val requirements: Set[Requirement] = Set(
      "A" -> Set(Constraint(binaryVersion, Set("1.0"))))

    (progress, info, requirements, CacheManager.create)
  }
  test("MOVE THIS TO VERSION ORDER 3") {
    //    VariantOrder.addBinaryVersions()
  }

  def addThenCommit(variant: Variant, repo: GitRepository, repoMetadata: Set[RepositoryInfo]): Commit = {
    val variantMetadata = VariantMetadata.fromVariant(variant)
    repo.add(variantMetadata.write(variant.id, repo))
    val commit = repo.commit("Added A")
    repo.add(VersionOrder.orderBinaryVersions(variant.id, repo, commit))
    repo.add(RepositoryMetadata(repoMetadata.toSeq).write(variant.id, variantMetadata.hash, repo))
    repo.commit("Order! Oooorder in the repo!")
  }

  test("MOVE THIS TO VERSION ORDER 2") {
    usingTmpDir { tmpDir =>
      val repoA = new GitRepository(tmpDir, RepositoryName("com.a"))
      repoA.init()
      val repoB = new GitRepository(tmpDir, RepositoryName("com.b"))
      repoB.init()
      val repoC = new GitRepository(tmpDir, RepositoryName("com.c"))
      repoC.init()

      val idA = Id("A")
      val idB = Id("B")
      val idC = Id("C")

      val variantA = Variant(idA, Set(version -> Set("1.0.0"), binaryVersion -> Set("1.0")))
      val commitA = addThenCommit(variantA, repoA, Set())
      val hashA = VariantMetadata.fromVariant(variantA).hash
      val repoInfoA = RepositoryInfo(idA, repoA.name, commitA, hashA)
      addThenCommit(Variant(idB, Set(version -> Set("1.0.0"), binaryVersion -> Set("1.0")),
        requirements = Set(Requirement(idA, Set.empty))), repoB,
        Set(repoInfoA))
      addThenCommit(Variant(idC, Set(version -> Set("1.0.0"), binaryVersion -> Set("1.0")),
        requirements = Set(Requirement(idA, Set.empty))), repoC,
        Set(repoInfoA))

      VersionOrder.useBinaryVersionOf(idA, repoA, commitA, inRepositories = Set(repoB, repoC)).foreach {
        case (repo, file) =>
          repo.add(file)
          repo.commit("Using binary version for: " + idA.value)
      }
      val activeBs = Order.activeVariants(idB, repoB, repoB.getHead)
      activeBs must have size (1)
      activeBs.map { hash =>
        val newVariant = VariantMetadata.read(idB, hash, repoB, repoB.getHead).value
        val requirements = newVariant.requirements.find(_.id == idA).value
        requirements.constraint(AttributeDefaults.BinaryVersionAttribute).values must be === Set("1.0")
      }
      val activeCs = Order.activeVariants(idC, repoC, repoC.getHead)
      activeCs must have size (1)
      activeCs.map { hash =>
        val newVariant = VariantMetadata.read(idC, hash, repoC, repoC.getHead).value
        val requirements = newVariant.requirements.find(_.id == idA).value
        requirements.constraint(AttributeDefaults.BinaryVersionAttribute).values must be === Set("1.0")
      }

    }
  }

  test("MOVE THIS TO VERSION ORDER 1") {
    usingTmpDir { tmpDir =>
      val id = Id("A")
      val variant101 = Variant(id, Set(version -> Set("1.0.1"), binaryVersion -> Set("1.0")))
      val variant100 = Variant(id, Set(version -> Set("1.0.0"), binaryVersion -> Set("1.0")))
      val variant102 = Variant(id, Set(version -> Set("1.0.2"), binaryVersion -> Set("1.0")))
      val variant111 = Variant(id, Set(version -> Set("1.1.1"), binaryVersion -> Set("1.1")))
      val variant110 = Variant(id, Set(version -> Set("1.1.0"), binaryVersion -> Set("1.1")))
      val variant112 = Variant(id, Set(version -> Set("1.1.2"), binaryVersion -> Set("1.1")))

      val repository = new GitRepository(tmpDir, RepositoryName("com.a"))
      repository.init()
      repository.add(VariantMetadata.fromVariant(variant101).write(id, repository))
      repository.add(VariantMetadata.fromVariant(variant100).write(id, repository))
      repository.add(VariantMetadata.fromVariant(variant110).write(id, repository))

      val commit1 = repository.commit("Adding some data")
      repository.add(VersionOrder.orderBinaryVersions(id, repository, commit1))
      val commit2 = repository.commit("Order! Oooorder in the repo!")
      Order.chosenVariants(id, Set.empty, repository, commit2) must be === Set(VariantMetadata.fromVariant(variant101).hash, VariantMetadata.fromVariant(variant110).hash)

      repository.add(VariantMetadata.fromVariant(variant102).write(id, repository))
      repository.add(VariantMetadata.fromVariant(variant111).write(id, repository))
      repository.add(VariantMetadata.fromVariant(variant112).write(id, repository))
      val commit3 = repository.commit("More data! Jej!")

      repository.add(VersionOrder.orderBinaryVersions(id, repository, commit3))
      val commit4 = repository.commit("Adept: Now with more order!")

      Order.chosenVariants(id, Set.empty, repository, commit4) must be === Set(VariantMetadata.fromVariant(variant112).hash, VariantMetadata.fromVariant(variant102).hash)
    }
  }

  //  test("Basic end-to-end test: add, load and resolve") {
  //    usingTmpDir { tmpDir =>
  //      val (progress, info, requirements, cacheManager) = basicSetup(tmpDir)
  //      val repositoryInfos = info.map {
  //        case (v, r) =>
  //          val metadata = VariantMetadata.fromVariant(v)
  //          r.add(metadata.write(v.id, r))
  //          r.add(Order.add(v.id, VariantSet(Set(metadata.hash)), r))
  //          val commit = r.commit("Adding: " + v.id)
  //
  //          RepositoryInfo(v.id, r.name, commit, VariantSet(Set(metadata.hash))) -> RepositoryLocations(Set.empty)
  //      }
  //      val loader = new GitLoader(tmpDir, repositoryInfos, progress, cacheManager)
  //      val result = resolve(requirements, loader)
  //      checkResolved(result, Set("A", "B"))
  //      checkVariants(result, "A", version -> Set("1.0.0"), binaryVersion -> Set("1.0"))
  //      checkVariants(result, "B", version -> Set("2.0.1"), binaryVersion -> Set("2.0"))
  //    }
  //  }
  //
  //  test("Basic end-to-end test: verify that using hash is static even when changing commits") {
  //    usingTmpDir { tmpDir =>
  //      val (progress, info, requirements, cacheManager) = basicSetup(tmpDir)
  //      val repositoryInfos = info.map {
  //        case (v, r) =>
  //          { //Adding 1 bad variant before
  //            val badVariant = v.copy(requirements = Set.empty)
  //            val badMetadata = VariantMetadata.fromVariant(badVariant)
  //            r.add(badMetadata.write(badVariant.id, r))
  //            r.add(Order.add(badVariant.id, VariantSet(Set(badMetadata.hash)), r))
  //            r.commit("Adding bad variant: " + badVariant.id)
  //          }
  //          val metadata = VariantMetadata.fromVariant(v)
  //          r.add(metadata.write(v.id, r))
  //          r.add(Order.add(v.id, VariantSet(Set(metadata.hash)), r))
  //          r.commit("Fixing: " + v.id)
  //
  //          val badCommit = { //And another bad variant after, but link to hash
  //            val badVariant = v.copy(requirements = Set.empty)
  //            val badMetadata = VariantMetadata.fromVariant(badVariant)
  //            r.add(badMetadata.write(badVariant.id, r))
  //            r.add(Order.add(badVariant.id, VariantSet(Set(badMetadata.hash)), r))
  //            r.commit("Adding ANOTHER bad variant: " + badVariant.id)
  //          }
  //          RepositoryInfo(v.id, r.name, badCommit, VariantSet(Set(metadata.hash))) -> RepositoryLocations(Set.empty)
  //      }
  //      val loader = new GitLoader(tmpDir, repositoryInfos, progress, cacheManager)
  //      val result = resolve(requirements, loader)
  //      checkResolved(result, Set("A", "B"))
  //      checkVariants(result, "A", version -> Set("1.0.0"), binaryVersion -> Set("1.0"))
  //      checkVariants(result, "B", version -> Set("2.0.1"), binaryVersion -> Set("2.0"))
  //    }
  //  }
  //
  //  def addThenCommit(v: Variant, r: GitRepository) = {
  //    val metadata = VariantMetadata.fromVariant(v)
  //    r.add(metadata.write(v.id, r))
  //    r.add(Order.add(v.id, VariantSet(Set(metadata.hash)), r))
  //    val commit = r.commit("Fixing: " + v.id)
  //
  //    RepositoryInfo(v.id, r.name, commit, VariantSet(Set(metadata.hash))) -> RepositoryLocations(Set.empty)
  //  }
  //
  //  test("Insert in repo") {
  //    usingTmpDir { tmpDir =>
  //
  //      val progress = new TextProgressMonitor()
  //      val repoA = new GitRepository(tmpDir, RepositoryName("com.a"), progress)
  //      repoA.init()
  //      val repoB = new GitRepository(tmpDir, RepositoryName("com.b"), progress)
  //      repoB.init()
  //
  //      val binaryVersionB = "2.0"
  //      val idB = "B"
  //      val variantA = Variant("A", Set(version -> Set("1.0.0"), binaryVersion -> Set("1.0")),
  //        requirements = Set(idB -> Set(Constraint(binaryVersion, Set(binaryVersionB)))))
  //
  //      val variantB = Variant(idB, Set(version -> Set("2.0.1"), binaryVersion -> Set(binaryVersionB)),
  //        requirements = Set.empty)
  //
  //      val requirements: Set[Requirement] = Set(
  //        "A" -> Set(Constraint(binaryVersion, Set("1.0"))))
  //
  //      val repositoriesA = addThenCommit(variantA, repoA)
  //      val repositoriesB = addThenCommit(variantB, repoB)
  //
  //      val cacheManager = CacheManager.create
  //
  //      val (RepositoryInfo(_, _, commitB: Commit, _), _) = repositoriesB
  //
  //      val newVariantB2 = Variant(idB, Set(version -> Set("3.0.1"), binaryVersion -> Set("3.0")),
  //        requirements = Set.empty)
  //       
  //      val commitB2 = Order.update(newVariantB2.id, repoB, commitB)(VersionOrder.versionReplaceLogic(newVariantB2, repoB, commitB)).map{ file =>
  //        repoB.add(file)
  //        repoB.commit("Added: " + newVariantB2.id)
  //      }.value
  //      
  //      
  //      val newVariantB3 = Variant(idB, Set(version -> Set("3.0.0"), binaryVersion -> Set("3.0")),
  //        requirements = Set.empty)
  //       
  //      val commitB3 = Order.update(newVariantB3.id, repoB, commitB2)(VersionOrder.versionReplaceLogic(newVariantB3, repoB, commitB)).map{ file =>
  //        repoB.add(file)
  //        repoB.commit("Added: " + newVariantB3.id)
  //      }.value
  //    }
  //  }
}