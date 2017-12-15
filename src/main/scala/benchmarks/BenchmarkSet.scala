package benchmarks

import edu.utexas.stac.Cost


object BenchmarkSet {
  def measureCost[A](f: => A): Long = {
    Cost.reset()
    f
    Cost.read()
  }
}
