package adept.ext

import org.scalatest.FunSpec
import org.scalatest.Matchers
import adept.repository.models._
import adept.repository.metadata._
import adept.repository._
import adept.resolution.models._
import net.sf.ehcache.CacheManager
import java.io.File
import org.scalatest.OptionValues._
import scala.util.matching.Regex

class VersionRankTest extends FunSpec with Matchers {
  import adept.test.FileUtils._
  import adept.test.ResolverUtils._
  import adept.test.OutputUtils._

  describe("Creating binary versions with excludes and use version as binary") {
    val idA = Id("A")
    def binaryVersion(variant: Variant) = {
      usingTmpDir { tmpDir =>
        val repoA = new GitRepository(tmpDir, RepositoryName("com.a"))
        repoA.init()

        val variantMetadata = VariantMetadata.fromVariant(variant)
        repoA.add(variantMetadata.write(variant.id, repoA))
        repoA.commit("Added: " + variant.id)

        val rankId = RankingMetadata.getXRankId(variant.id, repoA).headOption.value
        repoA.add(RankingMetadata(Seq(variantMetadata.hash)).write(variant.id, rankId, repoA))
        repoA.commit("Order: " + variant.id)

        val (addFiles, rmFiles) = VersionRank.useSemanticVersionRanking(idA,
          repoA, repoA.getHead,
          useVersionAsBinary = Set("2\\.9.*?".r),
          excludes = Set("2\\.8.*?".r))
        repoA.add(addFiles)
        repoA.rm(rmFiles)
        repoA.commit("SemVer")
        val activeAs = RankLogic.getActiveVariants(idA, repoA, repoA.getHead)
        activeAs should have size (1)
        val hash = activeAs.headOption.value
        VariantMetadata.read(idA, hash, repoA, repoA.getHead).value.toVariant(idA).attribute(AttributeDefaults.BinaryVersionAttribute).values
      }
    }

    it("should work for regular version strings") {
      binaryVersion(Variant(idA, Set(version -> Set("2.10.1")))) shouldEqual Set("2.10")
    }
    it("should work for more 'exotic' version strings") {
      binaryVersion(Variant(idA, Set(version -> Set("2.11.1-SNAPSHOT")))) shouldEqual Set("2.11")
    }
    it("should use versions as binaries for matching versions") {
      binaryVersion(Variant(idA, Set(version -> Set("2.9.3")))) shouldEqual Set("2.9.3")
    }
    it("should skip versions that matches excludes") {
      binaryVersion(Variant(idA, Set(version -> Set("2.8.1")))) shouldEqual Set()
    }
  }
  describe("Creating binary versions with includes, excludes and use version as binary") {
    val idA = Id("A")
    def binaryVersion(variant: Variant) = {
      usingTmpDir { tmpDir =>
        val repoA = new GitRepository(tmpDir, RepositoryName("com.a"))
        repoA.init()

        val variantMetadata = VariantMetadata.fromVariant(variant)
        repoA.add(variantMetadata.write(variant.id, repoA))
        repoA.commit("Added: " + variant.id)

        val rankId = RankingMetadata.getXRankId(variant.id, repoA).headOption.value
        repoA.add(RankingMetadata(Seq(variantMetadata.hash)).write(variant.id, rankId, repoA))
        repoA.commit("Order: " + variant.id)

        val (addFiles, rmFiles) = VersionRank.useSemanticVersionRanking(idA,
          repoA, repoA.getHead,
          includes = Set("2\\.10.*?".r, "2\\.11.*?".r),
          useVersionAsBinary = Set("2\\.9.*?".r),
          excludes = Set(".*".r))
        repoA.add(addFiles)
        repoA.rm(rmFiles)
        repoA.commit("SemVer")
        val activeAs = RankLogic.getActiveVariants(idA, repoA, repoA.getHead)
        activeAs should have size (1)
        val hash = activeAs.headOption.value
        VariantMetadata.read(idA, hash, repoA, repoA.getHead).value.toVariant(idA).attribute(AttributeDefaults.BinaryVersionAttribute).values
      }
    }

    it("should work for regular version strings") {
      binaryVersion(Variant(idA, Set(version -> Set("2.10.1")))) shouldEqual Set("2.10")
    }
    it("should work for more 'exotic' version strings") {
      binaryVersion(Variant(idA, Set(version -> Set("2.11.1-SNAPSHOT")))) shouldEqual Set("2.11")
    }
    it("should use versions as binaries for matching versions") {
      binaryVersion(Variant(idA, Set(version -> Set("2.9.3")))) shouldEqual Set("2.9.3")
    }
    it("should skip versions that matches excludes") {
      binaryVersion(Variant(idA, Set(version -> Set("2.8.1")))) shouldEqual Set()
    }
  }

  describe("Using binary versions in OTHER variants") {
    it("should replace the latest variant with the one that uses the binary version") {

      def addThenCommit(variant: Variant, repo: GitRepository, resolutionResults: Set[ResolutionResult]): Commit = {
        val variantMetadata = VariantMetadata.fromVariant(variant)
        repo.add(variantMetadata.write(variant.id, repo))
        repo.commit("Added: " + variant.id)
        val (addFiles, rmFiles) = VersionRank.useSemanticVersionRanking(variant.id, repo, repo.getHead)
        repo.add(addFiles)
        repo.add(rmFiles)
        repo.add(ResolutionResultsMetadata(resolutionResults.toSeq).write(variant.id, variantMetadata.hash, repo))
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
        val resolveResultA = ResolutionResult(idA, repoA.name, commitA, hashA)
        val commitB1 = addThenCommit(Variant(idB, Set(version -> Set("1.0.0"), binaryVersion -> Set("1.0")),
          requirements = Set(Requirement(idA, Set.empty, Set.empty))), repoB,
          Set(resolveResultA))
        val commitB2 = addThenCommit(Variant(idB, Set(version -> Set("1.0.0"), binaryVersion -> Set("1.0")),
          requirements = Set(Requirement(idA, Set.empty, Set.empty))), repoB,
          Set(resolveResultA))
        commitB1 shouldEqual commitB2
        addThenCommit(Variant(idC, Set(version -> Set("1.0.0"), binaryVersion -> Set("1.0")),
          requirements = Set(Requirement(idA, Set.empty, Set.empty))), repoC,
          Set(resolveResultA))

        //TODO: fix useBinaryVersionOf
//        VersionRank.useBinaryVersionOf(idA, repoA, commitA, inRepositories = Set(repoB, repoC)).foreach {
//          case (repo, file) =>
//            repo.add(file)
//            repo.commit("Using binary version for: " + idA.value)
//        }
//        val activeBs = RankLogic.getActiveVariants(idB, repoB, repoB.getHead)
//        activeBs should have size (1)
//        activeBs.foreach { hash =>
//          val newVariant = VariantMetadata.read(idB, hash, repoB, repoB.getHead).value
//          val requirements = newVariant.requirements.find(_.id == idA).value
//          requirements.constraint(AttributeDefaults.BinaryVersionAttribute).values shouldEqual Set("1.0")
//        }
//        val activeCs = RankLogic.getActiveVariants(idC, repoC, repoC.getHead)
//        activeCs should have size (1)
//        activeCs.foreach { hash =>
//          val newVariant = VariantMetadata.read(idC, hash, repoC, repoC.getHead).value
//          val requirements = newVariant.requirements.find(_.id == idA).value
//          requirements.constraint(AttributeDefaults.BinaryVersionAttribute).values shouldEqual Set("1.0")
//        }
      }
    }
  }

  describe("Variants") {
    it("should be automatically re-ordered if they have only versions") {
      usingTmpDir { tmpDir =>
        val id = Id("A")
        val variant101 = Variant(id, Set(version -> Set("1.0.1")))
        val variant100 = Variant(id, Set(version -> Set("1.0.0")))
        val variant102 = Variant(id, Set(version -> Set("1.0.2")))

        val repository = new GitRepository(tmpDir, RepositoryName("com.a"))
        repository.init()
        repository.add(VariantMetadata.fromVariant(variant101).write(id, repository))
        repository.add(VariantMetadata.fromVariant(variant100).write(id, repository))

        val commit1 = repository.commit("Adding some data")
        val (addFiles, rmFiles) = VersionRank.useSemanticVersionRanking(id, repository, commit1)
        repository.add(addFiles)
        repository.rm(rmFiles)
        val commit2 = repository.commit("Order! Oooorder in the repo!") //rank used to be called order, it is a funny comment though :)
        (for {
          hash <- RankLogic.getActiveVariants(id, repository, commit2).toSeq
          metadata <- VariantMetadata.read(id, hash, repository, repository.getHead).toSeq
          attribute <- metadata.attributes.toSeq if attribute.name == version
          version <- attribute.values
        } yield {
          version
        }) shouldEqual Seq("1.0.1")

        {
          val (tmpAddFiles, tmpRmFiles) = VersionRank.useSemanticVersionRanking(id, repository, commit2)
          repository.add(tmpAddFiles)
          repository.rm(tmpRmFiles)
          commit2 shouldEqual repository.commit("And some order") //no reason things should change
        }
        repository.add(VariantMetadata.fromVariant(variant102).write(id, repository))
        val commit4 = repository.commit("Add some data...")

        val (addFiles2, rmFiles2) = VersionRank.useSemanticVersionRanking(id, repository, commit4)
        repository.add(addFiles2)
        repository.rm(rmFiles2)
        val commit5 = repository.commit("And some order")
        (for {
          hash <- RankLogic.getActiveVariants(id, repository, commit5).toSeq
          metadata <- VariantMetadata.read(id, hash, repository, repository.getHead).toSeq
          attribute <- metadata.attributes.toSeq if attribute.name == version
          version <- attribute.values
        } yield {
          version
        }) shouldEqual Seq("1.0.2")
      }
    }
    it("should be automatically re-ordered if they have binary versions") {
      usingTmpDir { tmpDir =>
        val id = Id("A")
        val variant101 = Variant(id, Set(version -> Set("1.0.1"), binaryVersion -> Set("1.0")))
        val variant100 = Variant(id, Set(version -> Set("1.0.0"), binaryVersion -> Set("1.0")))
        val variant102 = Variant(id, Set(version -> Set("1.0.2"), binaryVersion -> Set("1.0")))
        val variant111 = Variant(id, Set(version -> Set("1.1.1"), binaryVersion -> Set("1.1")))
        val variant110 = Variant(id, Set(version -> Set("1.1.0"), binaryVersion -> Set("1.1")))
        val variant112 = Variant(id, Set(version -> Set("1.1.2"), binaryVersion -> Set("1.1")))

        val variant200 = Variant(id, Set(version -> Set("2.0.0"), binaryVersion -> Set("2.0")))

        val repository = new GitRepository(tmpDir, RepositoryName("com.a"))
        repository.init()
        repository.add(VariantMetadata.fromVariant(variant101).write(id, repository))
        repository.add(VariantMetadata.fromVariant(variant100).write(id, repository))
        repository.add(VariantMetadata.fromVariant(variant110).write(id, repository))
        repository.add(VariantMetadata.fromVariant(variant200).write(id, repository))

        val commit1 = repository.commit("Adding some data")
        val (addFiles1, rmFiles1) = VersionRank.useSemanticVersionRanking(id, repository, commit1)
        repository.add(addFiles1)
        repository.rm(rmFiles1)
        val commit2 = repository.commit("Order! Oooorder in the repo!")
        RankLogic.getActiveVariants(id, repository, commit2) shouldEqual Set(VariantMetadata.fromVariant(variant101).hash, VariantMetadata.fromVariant(variant110).hash, VariantMetadata.fromVariant(variant200).hash)

        repository.rm(repository.getVariantFile(id, VariantMetadata.fromVariant(variant200).hash))
        val commit3 = repository.commit("Remove some data...")

        repository.add(VariantMetadata.fromVariant(variant102).write(id, repository))
        repository.add(VariantMetadata.fromVariant(variant111).write(id, repository))
        repository.add(VariantMetadata.fromVariant(variant112).write(id, repository))
        val commit4 = repository.commit("Add some data...")

        val (addFiles2, rmFiles2) = VersionRank.useSemanticVersionRanking(id, repository, commit4)
        repository.add(addFiles2)
        repository.rm(rmFiles2)
        val commit5 = repository.commit("And some order")
        RankLogic.getActiveVariants(id, repository, commit5) shouldEqual Set(VariantMetadata.fromVariant(variant112).hash, VariantMetadata.fromVariant(variant102).hash)

        repository.add(VariantMetadata.fromVariant(variant200).write(id, repository))
        val commit6 = repository.commit("Re-added something we removed")
        val (addFiles3, rmFiles3) = VersionRank.useSemanticVersionRanking(id, repository, commit6)
        repository.add(addFiles3)
        repository.rm(rmFiles3)

        val commit7 = repository.commit("Adept: Now with more order!")

        RankLogic.getActiveVariants(id, repository, commit7) shouldEqual Set(VariantMetadata.fromVariant(variant112).hash, VariantMetadata.fromVariant(variant102).hash, VariantMetadata.fromVariant(variant200).hash)
      }
    }
  }

  describe("When importing variants from something that is version-based, VersionRank") {
    import adept.test.CacheUtils._
    it("should create resolution results for us") {
      val akkaId = "akka-actor_2.10"

      def insert(variant: Variant, repository: GitRepository) = {
        val metadata = VariantMetadata.fromVariant(variant)
        repository.add(metadata.write(variant.id, repository))
        val commit = repository.commit("Adding " + variant.id)

        val excludes =
          if (variant.id != Id(akkaId)) Set(".*".r) //do not add binary version for config and scala here, because it complicates the test
          else Set.empty[Regex]

        val (addFiles, rmFiles) = VersionRank.useSemanticVersionRanking(variant.id, repository, commit, excludes = excludes)
        repository.add(addFiles)
        repository.rm(rmFiles)
        repository.commit("Ranked " + variant.id)
      }

      usingTmpDir { tmpDir =>
        //***** SETUP START
        //Config ----
        val configRepository = new GitRepository(tmpDir, RepositoryName("com.typesafe"))
        configRepository.init()
        val configId = "config"
        insert(Variant(configId, Set(version -> Set("1.0.0")),
          requirements = Set.empty), configRepository)

        val configTargetVersion = "1.0.2"
        val configVariant = Variant(configId, Set(version -> Set(configTargetVersion)),
          requirements = Set.empty)
        insert(configVariant, configRepository)

        insert(Variant(configId, Set(version -> Set("1.1.0")),
          requirements = Set.empty), configRepository)
        //--- Config

        //Scala ---
        val scalaRepository = new GitRepository(tmpDir, RepositoryName("org.scala-lang"))
        scalaRepository.init()

        val scalaTargetVersion = "2.10.2"
        val scalaId = "scala-library"
        insert(Variant(scalaId, Set(version -> Set("2.10.1")),
          requirements = Set.empty), scalaRepository)

        val scalaVariant = Variant("scala-library", Set(version -> Set(scalaTargetVersion)),
          requirements = Set.empty)

        insert(scalaVariant, scalaRepository)

        insert(Variant(scalaId, Set(version -> Set("2.10.3")),
          requirements = Set.empty), scalaRepository)
        insert(Variant(scalaId, Set(version -> Set("2.9.3")),
          requirements = Set.empty), scalaRepository)

        //--- Scala

        //--- Akka
        val akkaRepository = new GitRepository(tmpDir, RepositoryName("com.typesafe.akka"))
        akkaRepository.init()
        val akkaTargetVersion = "2.2.0"
        val akkaVariant = Variant(akkaId, Set(version -> Set(akkaTargetVersion)),
          requirements = Set(
            configVariant.id.value -> Set.empty[Constraint],
            scalaVariant.id.value -> Set.empty[Constraint]))

        val versionInfo: Set[((RepositoryName, Id, VariantHash), Set[(RepositoryName, Id, Version)])] = Set(
          ((akkaRepository.name, akkaVariant.id, VariantMetadata.fromVariant(akkaVariant).hash),
            Set((configRepository.name, configVariant.id, Version(configTargetVersion)),
              (scalaRepository.name, scalaVariant.id, Version(scalaTargetVersion)))))

        versionInfo.foreach {
          case ((name, id, hash), versionInfo) =>
            val repository = new GitRepository(tmpDir, name)
            val (errors, results) = VersionRank.createResolutionResults(tmpDir, versionInfo)
            errors should have size(0)
            repository.add(ResolutionResultsMetadata(results.toSeq).write(id, hash, repository))
            repository.commit("Added resolution results from version map")
        }
        insert(akkaVariant, akkaRepository)

        //****** SETUP END 

        val inputRepostioryRequirements: Set[(RepositoryName, Requirement)] = Set(
          akkaRepository.name ->
            (akkaVariant.id.value -> Set.empty[Constraint]))
        val resolutionResults = GitLoader.getLatestResolutionResults(tmpDir, inputRepostioryRequirements, progress, cacheManager).map(_._1)
        val loader = new GitLoader(tmpDir, resolutionResults, progress, cacheManager)
        val result = resolve(inputRepostioryRequirements.map(_._2), loader)
        checkResolved(result, Set(akkaVariant.id, configVariant.id, scalaVariant.id))
        checkVariants(result, akkaVariant.id, version -> Set(akkaTargetVersion), binaryVersion -> Set(Version(akkaTargetVersion).asBinaryVersion))
        checkVariants(result, configVariant.id, version -> Set(configTargetVersion))
        checkVariants(result, scalaVariant.id, version -> Set(scalaTargetVersion))
      }
    }
  }

}