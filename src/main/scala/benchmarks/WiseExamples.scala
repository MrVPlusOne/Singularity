package benchmarks

import java.util.Random

import patsyn.Runner.RunnerConfig
import patsyn.StandardSystem._
import patsyn._

object WiseExamples {

  private def vecToJavaIntArray(vec: Vector[EValue]): Array[Integer] =
    vec.map { case IntValue(i) => new Integer(i) }.toArray

  private def simpleProblem[T](name: String, build: Array[Integer] => T)(func: (T, Integer) => Any) = ProblemConfig(
    name,
    outputTypes = IS(EVect(EInt), EInt),
    sizeF = {
      case IS(vec, _) =>
        vec.memoryUsage.toInt
    },
    resourceUsage = {
      case IS(VectValue(vec), IntValue(i)) =>
        val arr = vecToJavaIntArray(vec)
        val ds = build(arr)
        BenchmarkSet.measureCost {
          func(ds, new Integer(i))
        }
    }
  )

  private def sortProblem[T](name: String, build: Array[Integer] => T)(sort: T => Any) = ProblemConfig(
    name,
    outputTypes = IS(EVect(EInt)),
    sizeF = {
      case IS(vec) =>
        vec.memoryUsage.toInt
    },
    resourceUsage = {
      case IS(VectValue(vec)) =>
        val arr = vecToJavaIntArray(vec)
        val ds = build(arr)
        BenchmarkSet.measureCost {
          sort(ds)
        }
    }
  )

  private def translateAdjMatrix(graphValue: GraphValue, lowerWeight: Int, upperWeight: Int) = {
    val numNode = graphValue.nodeNum
    val matrix = Array.ofDim[Int](numNode, numNode)
    graphValue.edges.foreach {
      case (src, dst, weightValue) =>
        val rawWeight = weightValue.asInstanceOf[IntValue].value
        val weight = SimpleMath.wrapInRange(rawWeight, upperWeight - lowerWeight) + lowerWeight
        matrix(src)(dst) = weight
    }
    matrix
  }

  private def graphProblem(name: String, run: (Int, Array[Array[Int]]) => Any, lowerWeight: Int, upperWeight: Int) = ProblemConfig(
    name,
    outputTypes = IS(EGraph(EInt)),
    sizeF = {
      case IS(GraphValue(numNodes, _)) =>
        numNodes
    },
    resourceUsage = {
      case IS(g: GraphValue) =>
        val adjMatrix = translateAdjMatrix(g, lowerWeight, upperWeight)
        BenchmarkSet.measureCost {
          run(g.nodeNum, adjMatrix)
        }
    }
  )

  def listInsert = simpleProblem("wise.listInsert", patbench.wise.SortedListInsert.build)(patbench.wise.SortedListInsert.insert)
  def heapInsert = simpleProblem("wise.heapInsert", patbench.wise.HeapInsertJDK15.build)(patbench.wise.HeapInsertJDK15.insert)
  def bstSearch = simpleProblem("wise.bstSearch", patbench.wise.BinaryTreeSearch.build)(patbench.wise.BinaryTreeSearch.search)
  def rbSearch = simpleProblem("wise.rbSearch", patbench.wise.RedBlackTreeSearch.build)(patbench.wise.RedBlackTreeSearch.search)
  def quickSort = sortProblem("wise.quickSortJDK15", patbench.wise.QuickSortJDK15.build)(patbench.wise.QuickSortJDK15.sort)
  def mergeSort = sortProblem("wise.mergeSortJDK15", patbench.wise.MergeSortJDK15.build)(patbench.wise.MergeSortJDK15.sort)
  def dijkstra = graphProblem("wise.dijkstra", (n, d) => patbench.wise.Dijkstra.runDijkstra(n, d, 0), 0, 1000)
  def bellmanFord = graphProblem("wise.bellmanFord", (n, d) => patbench.wise.BellmanFord.runBellmanFord(n, d, 0), -1000, 1000)
  def tsp = graphProblem("wise.tsp", patbench.wise.Tsp.runTsp, 0, 1000)

  def allProblems = IS(listInsert, heapInsert, bstSearch, rbSearch, quickSort, mergeSort, dijkstra, bellmanFord, tsp)

  def runExample(seed: Int, useGUI: Boolean): Unit = {
    val rand = new Random(seed)
    Supernova.standardSupernova.fuzzProblem(
      tsp,
      RunnerConfig().copy(randomSeed = seed, ioId = seed, useGUI = useGUI),
      ExecutionConfig(evalSizePolicy = FixedEvalSize(100), timeLimitInMillis = 10000), rand,
      overrideGpConfig = config => config.copy(exprCostPenaltyBase = 0.95)
    )
  }

  def main(args: Array[String]): Unit = {
    runExample(0, true)
    //        val ind = FileInteraction.readMultiIndFromFile("results/textbook.quickSort3Way[performance=2320.0][ioId=0,seed=0](18-02-25-16:54:38)/bestIndividual.serialized", StandardSystem.funcMap)
    //        visual.PatternPlot.showResourceUsageChart(quickSort3Way, ind, 1000, 50)
  }
}
