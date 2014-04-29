package adept.ivy

import org.scalatest.FunSuite
import org.scalatest.Matchers
import adept.ext.Version
import adept.repository.models.VariantHash
import adept.repository.models.RepositoryName
import adept.repository.models.ResolutionResult
import adept.repository.models.Commit
import adept.repository.models.VariantHash
import adept.repository.metadata.ResolutionResultsMetadata
import java.io.File
import adept.repository.GitRepository
import adept.repository.GitLoader
import adept.repository.metadata.VariantMetadata
import adept.utils.Hasher
import net.sf.ehcache.CacheManager
import org.eclipse.jgit.lib.TextProgressMonitor
import adept.lockfile.Lockfile
import adept.artifact.models.Artifact
import adept.artifact.models.ArtifactHash
import adept.resolution.models.Variant
import adept.resolution.models.Constraint
import adept.resolution.models.Id
import adept.resolution.models.Requirement
import adept.ext.VersionScanner
import org.apache.ivy.core.module.descriptor.ModuleDescriptor
import org.apache.ivy.core.module.descriptor.{ Configuration => IvyConfiguration }
import org.apache.ivy.core.module.descriptor.DefaultModuleDescriptor
import org.apache.ivy.core.module.id.ModuleRevisionId
import org.apache.ivy.core.module.descriptor.ExcludeRule
import org.apache.ivy.core.module.descriptor.DefaultExcludeRule
import org.apache.ivy.core.module.descriptor.DefaultArtifact
import org.apache.ivy.core.module.id.ArtifactId
import org.apache.ivy.core.module.id.ModuleId
import org.apache.ivy.plugins.matcher.ExactPatternMatcher
import org.apache.ivy.core.module.descriptor.DefaultDependencyDescriptor
import adept.resolution.resolver.models.ResolvedResult
import adept.resolution.models.Attribute
import adept.test.TestDetails
import adept.ext.VersionRank
import adept.ivy.scalaspecific.ScalaBinaryVersionConverter
import adept.repository.metadata.RankingMetadata
import adept.repository.RankLogic
import adept.ext.MetadataUpdate
import adept.resolution.resolver.models.UnderconstrainedResult
import adept.test.IvyTestUtils
import org.eclipse.jgit.lib.ProgressMonitor
import adept.ext.AttributeDefaults
import adept.repository.metadata.RepositoryLocationsMetadata

object DELETEME {

  val scalaRepoName = RepositoryName("org.scala-lang")
  val scalaLibIds = Set(
    Id("org.scala-lang/scala-library/config/compile"),
    Id("org.scala-lang/scala-library/config/default"),
    Id("org.scala-lang/scala-library/config/javadoc"),
    Id("org.scala-lang/scala-library/config/master"),
    Id("org.scala-lang/scala-library/config/provided"),
    Id("org.scala-lang/scala-library/config/runtime"),
    Id("org.scala-lang/scala-library/config/sources"),
    Id("org.scala-lang/scala-library/config/system"),
    Id("org.scala-lang/scala-library"))

  def versionScala(baseDir: File, results: Set[IvyImportResult], progress: ProgressMonitor) = {
    val scalaRepo = new GitRepository(baseDir, scalaRepoName)
    def scalaCommit = scalaRepo.getHead //ugly, but we must check if repo exists before doing this

    val existingVariants = if (scalaRepo.exists) {
      scalaLibIds.flatMap { id =>
        if (scalaLibIds(id)) {
          Some(id -> VariantMetadata.listVariants(id, scalaRepo, scalaCommit))
        } else None
      }.toMap
    } else {
      Map.empty[Id, Set[VariantHash]]
    }

    val nonExistingScalaIvyImports = results.filter { result => //a bit more code than strictly needed, because it reads better
      val variant = result.variant
      if (scalaLibIds(variant.id)) {
        if (!existingVariants.isDefinedAt(variant.id)) {
          true //is a scala lib, but there are no existing variants for this Id
        } else {
          val thisHashExists = existingVariants(variant.id)
          val binaryVersions = variant.attribute(AttributeDefaults.BinaryVersionAttribute)
          if (binaryVersions.values.nonEmpty && thisHashExists(VariantMetadata.fromVariant(variant).hash)) {
            false //is scala lib but binary versions already exists or it already exists, so skip
          } else {
            true //is scala lib and does not exists or the one that exists does not have a binary version (only applied the first time)
          }
        }
      } else {
        false //is not scala lib, so skip
      }
    }

    if (nonExistingScalaIvyImports.nonEmpty) {
      IvyImportResultInserter.insertAsResolutionResults(baseDir, nonExistingScalaIvyImports, progress)

      var (addFiles, rmFiles) = Set.empty[File] -> Set.empty[File]
      nonExistingScalaIvyImports.foreach { nonExistingScalaIvyImports =>
        val scalaId = nonExistingScalaIvyImports.variant.id
        val (currentAddFiles, currentRmFiles) = VersionRank.useSemanticVersionRanking(scalaId, scalaRepo, scalaCommit, includes = Set("2\\.11.\\d+".r, "2\\.10.*".r, "2\\.9.*".r), excludes = Set(".*".r), useVersionAsBinary = Set("2\\.8.*".r, "2\\.7.*".r, "2\\.6.*".r, "2\\.5.*".r, "2\\.4.*".r, "2\\.3.*".r))
        addFiles ++= currentAddFiles
        rmFiles ++= currentRmFiles
      }
      scalaRepo.add(addFiles)
      scalaRepo.rm(rmFiles)
      if (!scalaRepo.isClean) {
        scalaRepo.commit("Versioned Scala")
        MetadataUpdate.updateRepositoryResolutionResults(scalaRepo)
        val commit = scalaRepo.commit("Update commits on resolution results within repository (required after semantic versioning)")
        commit != scalaCommit //return true if changed (new commit != from previous)
      } else false
    } else false
  }

  def get = {
    val ivy = IvyTestUtils.ivy
    ivy.configure(new File("src/test/resources/delete-sbt-plugin-ivy-settings.xml"))

    val ivyConverter = new IvyAdeptConverter(ivy, changing = false)
    val repoName = "com.typesafe.play"

    val ivyResults = ivyConverter.ivyImport(repoName, "sbt-plugin", "2.2.2", adept.test.OutputUtils.progress)
    if (ivyResults.isLeft) println(ivyResults)
    ivyResults.right.get

  }
}
class DELETEMETest extends FunSuite with Matchers {
  import adept.test.ResolverUtils._
  import adept.test.CacheUtils._
  import adept.test.FileUtils.usingTmpDir
  import adept.test.BenchmarkUtils._ //convert to benchmark hashes
  import adept.test.OutputUtils._
  import adept.test.EitherUtils._
  import adept.test.ArtifactUtils._
  import adept.test.IvyTestUtils.ivy

  import IvyConstants._
  import IvyUtils.withConfiguration
  import adept.ext.AttributeDefaults._

  test("DELETE THIS (used to import ivy)") {
    def getLocations(repository: RepositoryName) = {
      val adepthubHost = "git://localhost/"
      Set(adepthubHost + repository.value)
    }
    val tmpDir = new File("/Users/freekh/Projects/adepthub/adepthub")
    val ivy = IvyTestUtils.ivy
    ivy.configure(new File("src/test/resources/delete-sbt-plugin-ivy-settings.xml"))

    val ivyConverter = new IvyAdeptConverter(ivy, changing = false)
//    val repoName = "deletemeorg"
    val repoName = "com.typesafe.play"
    val ivyResults = {

//      val imported = ivyConverter.ivyImport(repoName, "deletemename", "2.2.2", progress)
      val imported = ivyConverter.ivyImport(repoName, "sbt-plugin", "2.2.2", progress)
      if (imported.isLeft) println(imported)
      val ivyResults = imported.right.get
      DELETEME.versionScala(tmpDir, ivyResults, progress)
      val newResults = ivyResults.map(
        ScalaBinaryVersionConverter.convertResultWithScalaBinaryVersion(_))
      newResults
    }

    val insertedResults = IvyImportResultInserter.insertAsResolutionResults(tmpDir, ivyResults, progress)
    val semverResults = insertedResults
      .filter { case result => result.repository == RepositoryName(repoName) }

    semverResults.foreach { result =>
      val name = result.repository
      val id = result.id
      val repository = new GitRepository(tmpDir, name)
      val commit = repository.getHead
      val (addFiles, rmFiles) = VersionRank.useSemanticVersionRanking(id, repository, commit, includes = Set(".*".r), excludes = Set.empty, useVersionAsBinary = Set.empty)
      repository.add(addFiles)
      repository.rm(rmFiles)
    }
    semverResults.map(_.repository).foreach { name =>
      val repository = new GitRepository(tmpDir, name)
      repository.commit("Semver")
    }

    ivyResults.map(_.repository).foreach { name =>
      val repository = new GitRepository(tmpDir, name)
      val commit = repository.getHead
      VariantMetadata.listIds(repository, commit).foreach { id =>
        RankingMetadata.listRankIds(id, repository, commit).foreach { rankId =>
          val ranking = RankingMetadata.read(id, rankId, repository, commit)
          ranking.toSeq.flatMap(_.variants).foreach { hash =>
            val values = ResolutionResultsMetadata.read(id, hash, repository, repository.getHead).toSeq.flatMap(_.values)
            values.foreach { dependendResult =>
              if (dependendResult.repository != name) {
                repository.add(RepositoryLocationsMetadata(getLocations(dependendResult.repository).toSeq).write(dependendResult.repository, repository))
              }
            }
          }
        }
      }
    }
    ivyResults.map(_.repository).toSet.foreach { name: RepositoryName =>
      val repository = new GitRepository(tmpDir, name)
      if (!repository.isClean)
        repository.commit("Added Adepthub repository locations")
    }
  }

}