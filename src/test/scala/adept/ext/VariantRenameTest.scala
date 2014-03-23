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

class VariantRenameTest extends FunSuite with Matchers {
  import adept.test.FileUtils._
  import adept.test.ResolverUtils._

  test("Basic variant rename") {
    usingTmpDir { tmpDir =>
//      val sourceId = Id("com.typesafe.akka/akka-actor_2.10")
//      val sourceRepository = new GitRepository(tmpDir, RepositoryName("com.typesafe.akka"))
//      sourceRepository.init()
//      val oldVariant1 = Variant(
//        id = sourceId,
//        attributes = Set(version -> Set("2.1.0"), binaryVersion -> Set("2.1")))
//      val oldVariant2 = Variant(
//        id = sourceId,
//        attributes = Set(version -> Set("2.2.0"), binaryVersion -> Set("2.2")))
//      sourceRepository.add(VariantMetadata.fromVariant(oldVariant1).write(sourceId, sourceRepository),
//        VariantMetadata.fromVariant(oldVariant2).write(sourceId, sourceRepository))
//      val oldVariantCommit = sourceRepository.commit("Adding some variants")
//      sourceRepository.add(VersionOrder.useDefaultVersionOrder(sourceId, sourceRepository, oldVariantCommit))
//      val sourceCommit = sourceRepository.commit("Ordered")
//
//      val sourceVariant = oldVariant1
//      val destId = Id("akka-actor")
//      val destRepository = new GitRepository(tmpDir, RepositoryName("akka"))
//
//      val ((sourceRepo, sourceFiles), (destRepo, destFiles)) =
//        VariantRename.rename(tmpDir, sourceId, sourceRepository.name, sourceCommit, destId, destRepository.name)
//
//      sourceRepo.add(sourceFiles)
//      sourceRepo.commit("Renamed to: " + destId)
//
//      destRepo.add(destFiles)
//      destRepo.commit("Renamed from " + sourceId)
//      
//      sourceFiles should have size (3) //1 variant file (the one that just redirects) and modify the 2 order files to use new variant file 
//      destFiles should have size (6) //TODO: fails but should be: 2 new variants, 1 old variants, 2 dest orderfiles, 1 source order files 
//      
//      { //check idempotency
//        val ((sourceRepo, sourceFiles), (destRepo, destFiles)) =
//
//          VariantRename.rename(tmpDir, sourceId, sourceRepository.name, sourceCommit, destId, destRepository.name)
//          sourceFiles should have size (0)
//          destFiles should have size (0)
//      }
    }
  }

}