package adept.repository

import org.scalatest.FunSuite
import org.scalatest.Matchers
import org.eclipse.jgit.lib.TextProgressMonitor
import adept.repository.models._
import adept.resolution.models._
import net.sf.ehcache.CacheManager
import adept.repository.serialization.Order
import java.io.File
import adept.ext.Version
import org.scalatest.OptionValues._
import adept.ext.VersionOrder
import adept.ext.AttributeDefaults

class RepositoryTest extends FunSuite with Matchers {
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