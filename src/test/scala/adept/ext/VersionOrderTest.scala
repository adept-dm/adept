package adept.ext

import org.scalatest.FunSpec
import org.scalatest.matchers.MustMatchers
import adept.repository.serialization.VariantMetadata
import adept.repository.models._
import adept.resolution.models._
import net.sf.ehcache.CacheManager
import adept.repository.serialization.Order
import java.io.File
import org.scalatest.OptionValues._
import adept.repository.serialization.RepositoryMetadata
import adept.repository._

class VersionOrderTest extends FunSpec with MustMatchers {
  import adept.test.FileUtils._
  import adept.test.ResolverUtils._

  describe("Creating binary versions") {
    val idA = Id("A")
    def binaryVersion(variant: Variant) = {
      usingTmpDir { tmpDir =>
        val repoA = new GitRepository(tmpDir, RepositoryName("com.a"))
        repoA.init()

        val variantMetadata = VariantMetadata.fromVariant(variant)
        repoA.add(variantMetadata.write(variant.id, repoA))
        repoA.commit("Added: " + variant.id)
        repoA.add(Order.insertNewFile(variant.id, variantMetadata.hash, repoA, repoA.getHead))
        repoA.commit("Order: " + variant.id)

        repoA.add(VersionOrder.useSemanticVersions(idA,
          VariantMetadata.fromVariant(variant).hash,
          repoA, repoA.getHead,
          useVersionAsBinary = Set("2\\.9.*?".r),
          excludes = Set("2\\.8.*?".r)))
        repoA.commit("SemVer")
        val activeAs = Order.activeVariants(idA, repoA, repoA.getHead)
        activeAs must have size (1)
        val hash = activeAs.headOption.value
        VariantMetadata.read(idA, hash, repoA, repoA.getHead).value.attribute(AttributeDefaults.BinaryVersionAttribute).values
      }
    }

    it("should work for regular version strings") {
      binaryVersion(Variant(idA, Set(version -> Set("2.10.1")))) must be === Set("2.10")
    }
    it("should work for more 'exotic' version strings") {
      binaryVersion(Variant(idA, Set(version -> Set("2.11.1-SNAPSHOT")))) must be === Set("2.11")
    }
    it("should use versions as binaries for matching versions") {
      binaryVersion(Variant(idA, Set(version -> Set("2.9.3")))) must be === Set("2.9.3")
    }
    it("should skip versions that matches excludes") {
      binaryVersion(Variant(idA, Set(version -> Set("2.8.1")))) must be === Set()
    }
  }

  describe("Using binary versions in OTHER variants") {
    it("should replace the latest variant with one that uses the binary version") {

      def addThenCommit(variant: Variant, repo: GitRepository, resovlveResults: Set[ResolveResult]): Commit = {
        val variantMetadata = VariantMetadata.fromVariant(variant)
        repo.add(variantMetadata.write(variant.id, repo))
        repo.commit("Added: " + variant.id)
        repo.add(VersionOrder.orderBinaryVersions(variant.id, repo, repo.getHead))
        repo.add(RepositoryMetadata(resovlveResults.toSeq).write(variant.id, variantMetadata.hash, repo))
        repo.commit("Order & repository metadata: " + variant.id)
      }

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
        val resolveResultA = ResolveResult(idA, repoA.name, commitA, hashA)
        addThenCommit(Variant(idB, Set(version -> Set("1.0.0"), binaryVersion -> Set("1.0")),
          requirements = Set(Requirement(idA, Set.empty))), repoB,
          Set(resolveResultA))
        addThenCommit(Variant(idC, Set(version -> Set("1.0.0"), binaryVersion -> Set("1.0")),
          requirements = Set(Requirement(idA, Set.empty))), repoC,
          Set(resolveResultA))

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
  }

  describe("Variants with binary versions") {
    it("should be automatically re-ordered by orderBinaryVersions") {
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
  }

}