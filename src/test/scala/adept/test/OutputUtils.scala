package adept.test

import org.eclipse.jgit.lib.TextProgressMonitor
import org.eclipse.jgit.lib.NullProgressMonitor

object OutputUtils {
  val printOnCommandline = true

  def progress = if (printOnCommandline) new TextProgressMonitor() else NullProgressMonitor.INSTANCE
  
  def benchmark[A](name: BenchmarkName, benchmarkId: BenchmarkId)(func: => A)(implicit testId: TestDetails): A = {
    val initTime = System.currentTimeMillis()
    val res = func
    if (printOnCommandline)
      Benchmarkers.systemErrBenchmarker.benchmark(name, System.currentTimeMillis() - initTime, benchmarkId)(testId)
    else
      Benchmarkers.nullBenchmarker.benchmark(name, System.currentTimeMillis() - initTime, benchmarkId)(testId)
    res
  }
  
}