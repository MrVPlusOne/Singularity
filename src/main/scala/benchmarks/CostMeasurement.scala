package benchmarks

import edu.utexas.stac.Cost


object CostMeasurement {
  def measureCost[A](f: => A): Long = {
    Cost.reset()
    f
    Cost.read()
  }

  def handleException[A](default: => A)(f: => A): A = {
    try {
      f
    } catch {
      case _: Exception => default
    }
  }
}
