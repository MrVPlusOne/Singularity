package singularity.examples

import singularity._
import singularity.visual.PatternPlot

object PatternRestoration {

  /** Restore and extrapolate the best input pattern found in the quickSort example */
  def main(args: Array[String]): Unit = {
    val config: ProblemConfig = QuickSort.quickSortProblem

    val patternFile = "results-running/quickSort example[ioId=0,seed=0](18-06-20-10:29:15)/bestIndividual.serialized"
    val individual = FileInteraction.readMultiIndFromFile(patternFile, StandardSystem.funcMap)

    PatternPlot.showResourceUsageChart(
      config,
      individual,
      sizeLimit = 1000, // extrapolation size
      plotPoints = 50 //number of data points in the displayed plot
    )
  }
}
