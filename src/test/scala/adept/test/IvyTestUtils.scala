package adept.test

import org.apache.ivy.core.module.descriptor.ModuleDescriptor
import adept.ivy.IvyAdeptConverter
import adept.ivy.IvyUtils
import adept.ivy.IvyImportResultInserter
import adept.ivy.IvyRequirements
import adept.ext.VersionRank
import adept.resolution.resolver.models.ResolvedResult
import java.io.File
import adept.repository.GitLoader
import org.apache.ivy.Ivy
import adept.ivy.scala.ScalaBinaryVersionConverter
import adept.repository.models.RepositoryName
import adept.resolution.models.Id

object IvyTestUtils {
  import adept.test.BenchmarkUtils._
  import adept.test.OutputUtils._
  import adept.test.EitherUtils._
  import adept.test.ResolverUtils._
  import adept.test.CacheUtils._

  def ivy = this.synchronized { //avoid parallel test messing up Ivy imports
    IvyUtils.load()
  }

  def verify(tmpDir: File, ivy: Ivy, ivyModule: ModuleDescriptor)(implicit testDetails: TestDetails) = {

    val ivyConverter = new IvyAdeptConverter(ivy)

    val exists = { (_: RepositoryName, _: Id) => true } //TODO:

    val (results, configuredVersionInfo) = benchmark(IvyImport, ivyModule) {
      val (results, configuredVersionInfo) = ivyConverter.loadAsIvyImportResults(ivyModule, progress).failOnLeft
      val newConfiguredVersionInfo = configuredVersionInfo.map {
        case (conf, versionInfo) =>
          conf -> ScalaBinaryVersionConverter.convertVersionInfoWithScalaBinaryVersion(versionInfo, exists)
      }
      val newResults = results.map(ScalaBinaryVersionConverter.convertResultWithScalaBinaryVersion(_, exists))
      newResults -> newConfiguredVersionInfo
    }

    val resolutionResults = benchmark(Inserted, results) {
      IvyImportResultInserter.insertAsResolutionResults(tmpDir, results, progress)
    }
    val resolutionResults2 = benchmark(InsertAfterInserted, results) {
      IvyImportResultInserter.insertAsResolutionResults(tmpDir, results, progress)
    }
    resolutionResults shouldEqual resolutionResults2

    val requirements = benchmark(Converted, ivyModule && results) {
      IvyRequirements.convertIvyAsRequirements(ivyModule, results)
    }

    for (confName <- requirements.keys) {
      val resolutionResults =
        VersionRank.createResolutionResults(tmpDir, configuredVersionInfo(confName))

      val loader = benchmark(Loaded, resolutionResults) {
        new GitLoader(tmpDir, resolutionResults, progress, cacheManager)
      }

      val result = benchmark(Resolved, requirements(confName) && loader) {
        resolve(requirements(confName), loader)
      }
      result match {
        case resolvedResult: ResolvedResult =>
          val verificationResult = benchmark(Verified, resolvedResult && ivyModule) {
            ivyConverter.verifyConversion(confName, ivyModule, resolvedResult)
          }
          println(result)
          assert(verificationResult.isRight, "Verification of " + confName + " failed:\n" + verificationResult)
        case _ =>
          assert(false, "Expected to be able to resolve Adept for " + confName + ". Got result:\n" + result)
      }
    }

  }
}