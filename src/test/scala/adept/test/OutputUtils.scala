package adept.test

import org.eclipse.jgit.lib.TextProgressMonitor
import org.eclipse.jgit.lib.NullProgressMonitor

object OutputUtils {
  val printOnCommandline = true

  def progress = if (printOnCommandline) new TextProgressMonitor() else NullProgressMonitor.INSTANCE
  def benchmark(name: String, timeSpentMillis: Long, hash: BenchmarkId)(implicit testId: TestDetails): Unit = {
    if (printOnCommandline)
      Benchmarkers.systemErrBenchmarker.benchmark(name, timeSpentMillis, hash)(testId)
    else
      Benchmarkers.nullBenchmarker.benchmark(name, timeSpentMillis, hash)(testId)
  }
}