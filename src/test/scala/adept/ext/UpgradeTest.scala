package adept.ext

import org.scalatest.FunSuite
import org.scalatest.Matchers
import adept.resolution.models.Id
import adept.repository.GitRepository
import adept.repository.models.RepositoryName
import adept.resolution.models.Variant
import adept.repository.serialization.VariantMetadata
import adept.lockfile.Lockfile
import adept.resolution.models.Requirement
import adept.repository.models.ResolutionResult
import adept.repository.GitLoader
import adept.resolution.models.Constraint
import adept.resolution.models.Attribute
import adept.repository.serialization.RankingMetadata
import adept.repository.RankLogic

class UpgradeTest extends FunSuite with Matchers {
  import adept.test.FileUtils._
  import adept.test.ResolverUtils._
  import adept.test.CacheUtils._
  import adept.test.OutputUtils._

  test("Basic compatible upgrades  of lockfiles for binary-compatible versions") {
    usingTmpDir { tmpDir =>
      val id = Id("com.typesafe.akka/akka-actor_2.10")
      val repository = new GitRepository(tmpDir, RepositoryName("com.typesafe.akka"))
      repository.init()
      val variant1 = Variant(
        id = id,
        attributes = Set(version -> Set("2.1.0"), binaryVersion -> Set("2.1")))
      val variant2 = Variant(
        id = id,
        attributes = Set(version -> Set("2.2.0"), binaryVersion -> Set("2.2")))
      val variant3 = Variant(
        id = id,
        attributes = Set(version -> Set("2.2.1"), binaryVersion -> Set("2.2")))
      repository.add(
        VariantMetadata.fromVariant(variant1).write(id, repository),
        VariantMetadata.fromVariant(variant2).write(id, repository),
        VariantMetadata.fromVariant(variant3).write(id, repository))
      val variantCommit = repository.commit("Adding some variants")
      val (addFiles, rmFiles) = VersionRank.useSemanticVersionRanking(id, repository, variantCommit)
      repository.add(addFiles)
      repository.rm(rmFiles)
      val commit = repository.commit("With rankings")

      val requirements = Set(Requirement(id, Set(binaryVersion -> Set("2.2")), Set.empty))
      val resolutionResults = Set(ResolutionResult(id, repository.name, commit, VariantMetadata.fromVariant(variant2).hash))
      val loader1 = new GitLoader(tmpDir, resolutionResults, progress, cacheManager)
      val result1 = resolve(requirements, loader1)
      checkResolved(result1, Set(id))
      checkAttributeVariants(result1, id, Attribute(version, Set("2.2.0"))) //because of resolution result

      val lockfile = Lockfile.create(tmpDir, requirements, resolutionResults, result1, cacheManager)
      val upgradedReqs = Upgrade.toLatestBinaryCompatible(tmpDir, lockfile.requirements.toSet)
      val loader2 = new GitLoader(tmpDir, upgradedReqs.map(_.toResolutionResult), progress, cacheManager)
      val result2 = resolve(upgradedReqs.map(_.toRequirement), loader2)
      checkResolved(result2, Set(id))
      checkAttributeVariants(result2, id, Attribute(version, Set("2.2.1"))) //because upgrade should bump to latest
    }
  }

  test("Basic compatible upgrades of lockfiles for non-binary-compatible version") {
    usingTmpDir { tmpDir =>
      val id = Id("com.typesafe.akka/akka-actor_2.10")
      val repository = new GitRepository(tmpDir, RepositoryName("com.typesafe.akka"))
      repository.init()
      val variant1 = Variant(
        id = id,
        attributes = Set(version -> Set("2.1.0")))
      val variant2 = Variant(
        id = id,
        attributes = Set(version -> Set("2.2.0")))
      val variant3 = Variant(
        id = id,
        attributes = Set(version -> Set("2.2.1")))
      repository.add(
        VariantMetadata.fromVariant(variant1).write(id, repository),
        VariantMetadata.fromVariant(variant2).write(id, repository),
        VariantMetadata.fromVariant(variant3).write(id, repository))
      val variantCommit = repository.commit("Adding some variants")
      val (addFiles, rmFiles) = VersionRank.useSemanticVersionRanking(id, repository, variantCommit, excludes = Set(".*".r)) //do not create binary versions for anything!
      repository.add(addFiles)
      repository.rm(rmFiles)
      val commit = repository.commit("With rankings")

      val requirements = Set(Requirement(id, Set.empty, Set.empty))
      val resolutionResults = Set(ResolutionResult(id, repository.name, commit, VariantMetadata.fromVariant(variant2).hash))
      val loader1 = new GitLoader(tmpDir, resolutionResults, progress, cacheManager)
      val result1 = resolve(requirements, loader1)
      checkResolved(result1, Set(id))
      checkAttributeVariants(result1, id, Attribute(version, Set("2.2.0"))) //because we set the hash

      val lockfile = Lockfile.create(tmpDir, requirements, resolutionResults, result1, cacheManager)
      val upgradedReqs = Upgrade.toLatestBinaryCompatible(tmpDir, lockfile.requirements.toSet)
      val loader2 = new GitLoader(tmpDir, upgradedReqs.map(_.toResolutionResult), progress, cacheManager)
      val result2 = resolve(upgradedReqs.map(_.toRequirement), loader2)
      checkResolved(result2, Set(id))
      checkAttributeVariants(result2, id, Attribute(version, Set("2.2.0"))) //because upgrade should not do anything
    }
  }
}