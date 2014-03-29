package adept.ext

import org.scalatest.FunSuite
import org.scalatest.Matchers
import adept.repository.GitRepository
import java.io.File
import adept.repository.models.RepositoryName
import adept.repository.metadata.VariantMetadata
import adept.repository.models.RepositoryLocations
import adept.repository.metadata.RepositoryLocationsMetadata
import org.eclipse.jgit.api.Git
import org.eclipse.jgit.lib.ConfigConstants
import adept.resolution.models.Variant
import adept.resolution.models.Id
import adept.resolution.models.Requirement
import adept.repository.GitLoader
import adept.resolution.Resolver
import adept.resolution.models.Constraint

class VariantRenameTest extends FunSuite with Matchers {
  import adept.test.FileUtils._
  import adept.test.ResolverUtils._
  import adept.test.CacheUtils._
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
      val oldVariant3 = Variant(
        id = sourceId,
        attributes = Set(version -> Set("2.2.1"), binaryVersion -> Set("2.2")))
      sourceRepository.add(
        VariantMetadata.fromVariant(oldVariant1).write(sourceId, sourceRepository),
        VariantMetadata.fromVariant(oldVariant2).write(sourceId, sourceRepository),
        VariantMetadata.fromVariant(oldVariant3).write(sourceId, sourceRepository))
      val oldVariantCommit = sourceRepository.commit("Adding some variants")
      val (addFiles, rmFiles) = VersionRank.useSemanticVersionRanking(sourceId, sourceRepository, oldVariantCommit)
      sourceRepository.add(addFiles)
      sourceRepository.rm(rmFiles)
      val sourceCommit = sourceRepository.commit("Ordered")

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
      val sourceReq = Requirement(sourceId, Set.empty, Set.empty)
      val destReq = Requirement(destId, Set(Constraint(binaryVersion, Set("2.2"))), Set.empty)

      withClue("Try resolution on source that has been renamed") {
        val resolutionResults = GitLoader.getLatestResolutionResults(tmpDir, Set(sourceRepository.name -> sourceReq), progress, cacheManager)
          .map(_._1)
        val loader = new GitLoader(tmpDir, resolutionResults, progress, cacheManager)
        val resolver = new Resolver(loader)
        val result = resolver.resolve(Set(sourceReq))
        checkResolved(result, Set(sourceId))
        checkUnresolved(result, Set(destId)) //you cannot resolve this because it has been renamed
      }

      withClue("Try resolution on new dest") {
        val resolutionResults = GitLoader.getLatestResolutionResults(tmpDir, Set(destRepository.name -> destReq), progress, cacheManager)
          .map(_._1)
        val loader = new GitLoader(tmpDir, resolutionResults, progress, cacheManager)
        val resolver = new Resolver(loader)
        val result = resolver.resolve(Set(destReq))
        assert(result.isResolved)  //should be resolved
        checkResolved(result, Set(sourceId, destId)) 
        checkUnresolved(result, Set())
      }

      withClue("Try resolution when both source and dest is required") {
        val resolutionResults = GitLoader.getResolutionResults(tmpDir, Set((destRepository.name, destReq, destRepository.getHead), (sourceRepository.name, sourceReq, sourceCommit)), progress, cacheManager)
          .map(_._1)
        val loader = new GitLoader(tmpDir, resolutionResults, progress, cacheManager)
        val resolver = new Resolver(loader)
        val result = resolver.resolve(Set(destReq))
        checkResolved(result, Set(destId))
        checkUnresolved(result, Set(sourceId)) //we are under-constrained because we are requiring something which has been reordered
      }
    }
  }

}