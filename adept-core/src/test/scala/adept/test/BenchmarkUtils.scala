package adept.test

import adept.resolution.models.Requirement
import org.apache.ivy.core.module.descriptor.ModuleDescriptor
import adept.repository.models.ResolutionResult
import adept.utils.Hasher
import adept.ivy.IvyImportResult
import adept.resolution.resolver.models.ResolveResult
import adept.resolution.models.Variant
import adept.repository.VariantsLoader
import adept.repository.GitLoader
import adept.repository.MemoryLoader

case class TestDetails(id: String)
case class BenchmarkId(id: String) extends AnyVal {
  def &&(other: BenchmarkId) = {
    BenchmarkId(id + other.id)
  }
}
case class BenchmarkName(value: String)

object BenchmarkUtils {
  val Inserted = BenchmarkName("Inserted")
  val InsertAfterInserted = BenchmarkName("InsertAfterInserted")
  val IvyImport = BenchmarkName("Ivy-import")
  val Converted = BenchmarkName("Converted")
  val Loaded = BenchmarkName("Loaded")
  val Resolved = BenchmarkName("Resolved")
  val Verified = BenchmarkName("Verified")

  import scala.language.implicitConversions //it is OK we are in test configuration only

  //TODO: move all Ivy things including this one to some other project?
  implicit def convertIvyModule(ivyModule: ModuleDescriptor): BenchmarkId = {
    BenchmarkId(ivyModule.toString)
  }

  implicit def convertIvyModule(results: Set[IvyImportResult]): BenchmarkId = {
    BenchmarkId(results.toString)
  }

  implicit def convertRequirements(requirements: Set[Requirement]): BenchmarkId = {
    BenchmarkId(requirements.toString)
  }

  implicit def convertResults(results: Set[ResolutionResult]): BenchmarkId = {
    BenchmarkId(results.toString)
    
  }
  implicit def convertGitLoader(loader: GitLoader): BenchmarkId = {
    convertResults(loader.results)
  }

  implicit def convertMemGitLoader(loader: MemoryLoader): BenchmarkId = {
    BenchmarkId(loader.variants.toString)
  }

  implicit def convertResolveResult(resolveResult: ResolveResult): BenchmarkId = {
    BenchmarkId(resolveResult.toString)
  }
}

object Benchmarkers {
  //Use OutputUtils
  private[test] val nullBenchmarker = new Benchmarker {
    override def benchmark(name: BenchmarkName, timeSpentMillis: Long, hash: BenchmarkId)(implicit testDetails: TestDetails): Unit = {}
  }
  private[test] val systemErrBenchmarker = new Benchmarker {
    override def benchmark(name: BenchmarkName, timeSpentMillis: Long, hash: BenchmarkId)(implicit testDetails: TestDetails): Unit = {
      System.err.println("Completed task: '" + name.value + "' (" + testDetails.id + ") in " + (timeSpentMillis / 1000.0) + "s")
    }
  }
}

abstract class Benchmarker {
  def benchmark(name: BenchmarkName, timeSpentMillis: Long, hash: BenchmarkId)(implicit testId: TestDetails): Unit
}