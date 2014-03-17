package adept.ivy

import org.scalatest.FunSuite
import org.scalatest.Matchers
import adept.ext.Version
import adept.repository.models.VariantHash
import adept.repository.models.RepositoryName
import adept.repository.models.ResolutionResult
import adept.repository.models.Commit
import adept.repository.models.VariantHash
import adept.repository.serialization.ResolutionResultsMetadata
import java.io.File
import adept.repository.GitRepository
import adept.repository.GitLoader
import adept.ext.VersionOrder
import adept.repository.serialization.Order
import adept.repository.serialization.VariantMetadata
import adept.utils.Hasher
import net.sf.ehcache.CacheManager
import org.eclipse.jgit.lib.TextProgressMonitor
import adept.resolution.models.Variant
import adept.resolution.models.Constraint
import adept.resolution.models.Id
import adept.resolution.models.Requirement
import adept.ext.VersionScanner

class IvyInserterTest extends FunSuite with Matchers {
  import adept.test.ResolverUtils._
  import adept.test.LoaderUtils._
  import adept.test.FileUtils.usingTmpDir

  def insert(variant: Variant, repository: GitRepository) = {
    val metadata = VariantMetadata.fromVariant(variant)
    repository.add(metadata.write(variant.id, repository))
    val commit = repository.commit("Adding " + variant.id)

    repository.add(VersionOrder.useDefaultVersionOrder(variant.id, repository, commit))
    repository.commit("Ordered " + variant.id)
  }

  test("Basics: verify that IvyInserter can add ivy results") {
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
      val akkaId = "akka-actor"
      val akkaTargetVersion = "2.2.0"
      val akkaVariant = Variant(akkaId, Set(version -> Set(akkaTargetVersion)),
        requirements = Set(
          configVariant.id.value -> Set.empty[Constraint],
          scalaVariant.id.value -> Set.empty[Constraint]))
      insert(akkaVariant, akkaRepository)

      //****** SETUP END 

      val versionInfo: Set[((RepositoryName, Id, VariantHash), Set[(RepositoryName, Id, Version)])] = Set(
        ((akkaRepository.name, akkaVariant.id, VariantMetadata.fromVariant(akkaVariant).hash),
          Set((configRepository.name, configVariant.id, Version(configTargetVersion)),
            (scalaRepository.name, scalaVariant.id, Version(scalaTargetVersion)))))

      val versionResolutionResults = versionInfo.map {
        case ((name, id, hash), dependencies) =>
          val resolutionResults = dependencies.map {
            case (targetName, targetId, targetVersion) =>
              val repository = new GitRepository(tmpDir, targetName)
              val commit = repository.getHead

              VersionScanner.findVersion(targetId, targetVersion, repository, commit) match {
                case Some(targetHash) =>
                  val result = ResolutionResult(targetId, targetName, commit, targetHash)
                  result
                case None => throw new Exception("Could not find: " + targetVersion + " for " + targetId + " in " + targetName + "")
              }
          }
          (name, id, hash) -> resolutionResults
      }

      val updatedRepositories = versionResolutionResults.flatMap {
        case ((name, id, hash), resolutionResults) =>
          if (resolutionResults.nonEmpty) {
            val repository = new GitRepository(tmpDir, name)
            repository.add(ResolutionResultsMetadata(resolutionResults.toSeq).write(id, hash, repository))
            Some(repository)
          } else None
      }

      updatedRepositories.foreach {
        _.commit("Added resolution results from version map")
      }

      val inputRepostioryRequirements: Set[(RepositoryName, Requirement)] = Set(
        akkaRepository.name ->
          (akkaVariant.id.value -> Set.empty[Constraint]))
      val resolutionResults = GitLoader.getLatestResolutionResults(tmpDir, inputRepostioryRequirements, progress, cacheManager).map(_._1)

      val loader = new GitLoader(tmpDir, resolutionResults, progress, cacheManager)
      val result = resolve(inputRepostioryRequirements.map(_._2), loader)
      checkResolved(result, Set(akkaVariant.id, configVariant.id, scalaVariant.id))
      checkVariants(result, akkaVariant.id, version -> Set(akkaTargetVersion))
      checkVariants(result, configVariant.id, version -> Set(configTargetVersion))
      checkVariants(result, scalaVariant.id, version -> Set(scalaTargetVersion))
    }
  }
}