package adept.repository

import org.scalatest.FunSuite
import org.scalatest.Matchers
import org.eclipse.jgit.lib.TextProgressMonitor
import adept.repository.models._
import adept.resolution.models._
import net.sf.ehcache.CacheManager
import java.io.File
import org.scalatest.OptionValues._
import adept.repository.metadata.VariantMetadata
import adept.repository.metadata.ResolutionResultsMetadata
import adept.repository.metadata.RepositoryLocationsMetadata
import adept.repository.metadata.RankingMetadata

class GitLoaderTest extends FunSuite with Matchers {
  import adept.test.FileUtils._
  import adept.test.ResolverUtils._
  import adept.test.CacheUtils._
  import adept.test.OutputUtils._

  def createVersionedResolutionResults(tmpDir: File) = {
    val repoA = new GitRepository(tmpDir, RepositoryName("com.a"))
    repoA.init()
    val repoB = new GitRepository(tmpDir, RepositoryName("com.b"))
    repoB.init()
    val aId = Id("A")
    val aVariant =  Variant(aId, Set(version -> Set("1.0.0"), binaryVersion -> Set("1.0")), requirements = Set("B" -> Set(Constraint(binaryVersion, Set("2.0")))))
    val aVariantHash = VariantMetadata.fromVariant(aVariant).hash
    val b20Variant = Variant("B", Set(version -> Set("2.0.1"), binaryVersion -> Set("2.0")), requirements = Set.empty)
    val b20VariantHash = VariantMetadata.fromVariant(b20Variant).hash
      
    val infoA = aVariant -> repoA
    val infoB = Set(
      Variant("B", Set(version -> Set("1.0.0"), binaryVersion -> Set("1.0")),
        requirements = Set.empty) -> repoB,
      b20Variant -> repoB)
        
    val results= (infoB + infoA).map {
      case (v, r) =>
        val metadata = VariantMetadata.fromVariant(v)
        r.add(metadata.write(v.id, r))
        val rankId = RankingMetadata.DefaultRankId
        r.add(RankingMetadata(Seq(metadata.hash)).write(v.id, rankId, r))
        val commit = r.commit("Adding: " + v.id)

        ResolutionResult(v.id, r.name, Some(commit), metadata.hash)
    }

    val aTransitiveResults = ResolutionResultsMetadata(results.toSeq.filter(_.variant == b20VariantHash))
    repoA.add(aTransitiveResults.write(aId, aVariantHash, repoA))
        
    results
  }

  def createUnversionedResolutionResults(tmpDir: File) = {
    val repo = new Repository(tmpDir, RepositoryName("com.b"))
    val info = Set(
      Variant("B", Set(version -> Set("1.0.1"), binaryVersion -> Set("1.0"))), 
      Variant("B", Set(version -> Set("1.0.0"), binaryVersion -> Set("1.0"))))

    val initialResults = info.map {
      case v =>
        val metadata = VariantMetadata.fromVariant(v)
        metadata.write(v.id, repo)
        val rankId = RankingMetadata.DefaultRankId
        RankingMetadata(Seq(metadata.hash)).write(v.id, rankId, repo)

        ResolutionResult(v.id, repo.name, None, metadata.hash)
    }
    initialResults
  }

  test("Git Loader basics: add and resolve") {
    usingTmpDir { tmpDir =>

      val requirements: Set[Requirement] = Set(
        "A" -> Set(Constraint(binaryVersion, Set("1.0"))))
      val initialResults = createVersionedResolutionResults(tmpDir)
      val loader = new GitLoader(tmpDir, initialResults, cacheManager, progress = progress)
      val result = resolve(requirements, loader)
      checkResolved(result, Set("A", "B"))
      checkVariants(result, "A", version -> Set("1.0.0"), binaryVersion -> Set("1.0"))
      checkVariants(result, "B", version -> Set("2.0.1"), binaryVersion -> Set("2.0"))
    }
  }
}