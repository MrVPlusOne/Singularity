package benchmarks

import java.util.Random

import patsyn.Runner.RunnerConfig
import patsyn.StandardSystem._
import patsyn._

object TextbookExamples {

  private def vecToJavaIntArray(vec: Vector[EValue]): Array[Comparable[_]] =
    vec.map { case IntValue(i) => new Integer(i) }.toArray

  private def intArrayProblem(name: String, func: Array[Comparable[_]] => Any) =
    ProblemConfig(
      name,
      outputTypes = IS(EVect(EInt)),
      sizeF = {
        case IS(vec) =>
          vec.memoryUsage.toInt
      },
      resourceUsage = {
        case IS(VectValue(vec)) =>
          val arr = vecToJavaIntArray(vec)
          BenchmarkSet.measureCost {
            func(arr)
          }
      }
    )

  def insertSort = intArrayProblem("textbook.insertSort", patbench.textbook.Insertion.sort)
  def insertSortOpt = intArrayProblem("textbook.insertSortOpt", patbench.textbook.InsertionX.sort)
  def insertSortBinary = intArrayProblem("textbook.insertSortBinary", patbench.textbook.BinaryInsertion.sort)
  def selectSort = intArrayProblem("textbook.selectSort", patbench.textbook.Selection.sort)
  def shellSort = intArrayProblem("textbook.shellSort", patbench.textbook.Shell.sort)
  def mergeSortTD = intArrayProblem("textbook.mergeSortTopDown", patbench.textbook.Merge.sort)
  def mergeSortBU = intArrayProblem("textbook.mergeSortBottomUp", patbench.textbook.MergeBU.sort)
  def mergeSortOpt = intArrayProblem("textbook.mergeSortX", patbench.textbook.MergeX.sort)
  def quickSort = intArrayProblem("textbook.quickSort", patbench.textbook.Quick.sort)
  def quickSort3Way = intArrayProblem("textbook.quickSort3Way", patbench.textbook.Quick3way.sort)
  def quickSortOpt = intArrayProblem("textbook.quickSortOpt", patbench.textbook.QuickX.sort)
  def quickSort3WayOpt = intArrayProblem("textbook.quickSort3WayOpt", patbench.textbook.QuickBentleyMcIlroy.sort)
  def heapSort = intArrayProblem("textbook.heapSort", patbench.textbook.Heap.sort)



  def runExample(seed: Int, useGUI: Boolean): Unit = {
    val rand = new Random(seed)
    Supernova.standardSupernova.fuzzProblem(
      quickSort3Way,
      RunnerConfig().copy(randomSeed = seed, ioId = seed, useGUI = useGUI),
      ExecutionConfig(evalSizePolicy = FixedEvalSize(100), timeLimitInMillis = 10000), rand,
      overrideGpConfig = config => config.copy(exprCostPenaltyBase = 1)
    )
  }

  def main(args: Array[String]): Unit = {
    runExample(0, true)
//        val ind = FileInteraction.readMultiIndFromFile("results/textbook.quickSort3Way[performance=2320.0][ioId=0,seed=0](18-02-25-16:54:38)/bestIndividual.serialized", StandardSystem.funcMap)
//        visual.PatternPlot.showResourceUsageChart(quickSort3Way, ind, 1000, 50)
  }
}
