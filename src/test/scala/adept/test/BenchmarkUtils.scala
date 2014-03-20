package adept.test

import adept.resolution.models.Requirement
import org.apache.ivy.core.module.descriptor.ModuleDescriptor
import adept.repository.models.ResolutionResult
import adept.utils.Hasher

case class TestDetails(id: String)
case class BenchmarkId(id: String) extends AnyVal

object BenchmarkUtils {

  val defaultBencharMarker = Benchmarkers.systemErrBenchmarker
  //TODO: move all Ivy things including this one to some other project?
  implicit def convertIvyModule(ivyModule: ModuleDescriptor): BenchmarkId = {
    BenchmarkId(ivyModule.toString)
  }

  implicit def convertRequirements(requirements: Set[Requirement]): BenchmarkId = {
    BenchmarkId(requirements.toString)
  }

  implicit def convertResolutionResults(resolutionResults: Set[ResolutionResult]): BenchmarkId = {
    BenchmarkId(resolutionResults.toString)
  }
}

object Benchmarkers {
  val nullBenchmarker = new Benchmarker {
    override def benchmark(name: String, timeSpentMillis: Long, hash: BenchmarkId)(implicit testId: TestDetails): Unit = {}
  }
  val systemErrBenchmarker = new Benchmarker {
    override def benchmark(name: String, timeSpentMillis: Long, hash: BenchmarkId)(implicit testId: TestDetails): Unit = {
      System.err.println("Completed task: '" + name + "' in " + (timeSpentMillis / 1000.0) + "s")
    }
  }
}

abstract class Benchmarker {
  def benchmark(name: String, timeSpentMillis: Long, hash: BenchmarkId)(implicit testId: TestDetails): Unit
}