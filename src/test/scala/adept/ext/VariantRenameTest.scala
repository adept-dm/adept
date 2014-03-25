package adept.ext

import org.scalatest.FunSuite
import org.scalatest.Matchers
import adept.repository.GitRepository
import java.io.File
import adept.repository.models.RepositoryName
import adept.repository.serialization.VariantMetadata
import adept.repository.models.RepositoryLocations
import adept.repository.serialization.RepositoryLocationsMetadata
import org.eclipse.jgit.api.Git
import org.eclipse.jgit.lib.ConfigConstants
import adept.resolution.models.Variant
import adept.resolution.models.Id
import adept.repository.GitLoader
import adept.resolution.models.Requirement
import adept.resolution.Resolver
import adept.repository.models.ResolutionResult
import adept.resolution.models.Constraint

class VariantRenameTest extends FunSuite with Matchers {
  import adept.test.FileUtils._
  import adept.test.ResolverUtils._
  import adept.test.LoaderUtils._
  import adept.test.OutputUtils._

  test("Basic variant rename") {
    usingTmpDir { tmpDir =>
      val sourceId = Id("com.typesafe.akka/akka-actor_2.10")
      val sourceRepository = new GitRepository(tmpDir, RepositoryName("com.typesafe.akka"))
      sourceRepository.init()
      val oldVariant1 = Variant(
        id = sourceId,
        attributes = Set(version -> Set("2.1.0"), binaryVersion -> Set("2.1")))
      val oldVariant2 = Variant(
        id = sourceId,
        attributes = Set(version -> Set("2.2.0"), binaryVersion -> Set("2.2")))
      sourceRepository.add(VariantMetadata.fromVariant(oldVariant1).write(sourceId, sourceRepository),
        VariantMetadata.fromVariant(oldVariant2).write(sourceId, sourceRepository))
      val oldVariantCommit = sourceRepository.commit("Adding some variants")
      val (addFiles, rmFiles) = VersionRank.useDefaultVersionRanking(sourceId, sourceRepository, oldVariantCommit)
      sourceRepository.add(addFiles)
      sourceRepository.rm(rmFiles)
      val sourceCommit = sourceRepository.commit("Ordered")

      val sourceVariant = oldVariant1
      val destId = Id("akka-actor")
      val destRepository = new GitRepository(tmpDir, RepositoryName("akka"))
      destRepository.init()

      val (sourceCommit1, destCommit1) =
        VariantRename.rename(tmpDir, sourceId, sourceRepository.name, sourceCommit, destId, destRepository.name)

      val (sourceCommit2, destCommit2) =
        VariantRename.rename(tmpDir, sourceId, sourceRepository.name, sourceCommit, destId, destRepository.name)

      assert(sourceCommit1 == sourceCommit2, "Rename should be idempotent but we got a new commit: " + sourceCommit2 + " when we already had: " + sourceCommit1)
      assert(destCommit1 == destCommit2, "Rename should be idempotent but we got a new commit: " + destCommit2 + " when we already had: " + destCommit1)

      //check resolution
      {
        val sourceReq = Requirement(sourceId, Set.empty, Set.empty)
        val resolutionResults = GitLoader.getLatestResolutionResults(tmpDir, Set(sourceRepository.name -> sourceReq), progress, cacheManager)
          .map(_._1)
        println(resolutionResults)
        val loader = new GitLoader(tmpDir, resolutionResults, progress, cacheManager)
        val resolver = new Resolver(loader)
        println(resolver.resolve(Set(sourceReq)))
      }

      {
        val sourceReq = Requirement(sourceId, Set.empty, Set.empty)
        val destReq = Requirement(destId, Set(Constraint(binaryVersion, Set("2.1"))), Set.empty)
        val resolutionResults = GitLoader.getLatestResolutionResults(tmpDir, Set(sourceRepository.name -> sourceReq), progress, cacheManager)
          .map(_._1)
        println(resolutionResults)
        val loader = new GitLoader(tmpDir, resolutionResults, progress, cacheManager)
        val resolver = new Resolver(loader)
        println(resolver.resolve(Set(sourceReq)))
      }
    }

  }

}