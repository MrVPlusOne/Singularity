package singularity.benchmarks

import singularity.measure.TimeTools
import singularity.Runner.RunnerConfig
import singularity.StandardSystem._
import singularity._

import scala.util.Random

object WiseExamples {

  private def vecToJavaIntArray(vec: Vector[EValue]): Array[Integer] =
    vec.map { case IntValue(i) => new Integer(i) }.toArray

  private def simpleProblem[T](name: String, build: Array[Integer] => T)(func: (T, Integer) => Any) = ProblemConfig(
    name,
    outputTypes = IS(EVect(EInt), EInt),
    sizeF = {
      case IS(VectValue(vec), _) =>
        vec.size
    },
    resourceUsage = {
      case IS(VectValue(vec), IntValue(i)) =>
        val arr = vecToJavaIntArray(vec)
        val ds = build(arr)
        BenchmarkSet.measureCost {
          func(ds, new Integer(i))
        }
    },
    saveValueWithName = (value: IS[EValue], name: String) => {
      value match {
        case IS(VectValue(v), IntValue(i)) =>
          val str = (v.map { case IntValue(j) => j } :+ i).mkString("\n")
          val textFileName = s"$name.raw.txt"
          FileInteraction.writeToFile(textFileName)(str)
      }
    }
  )

  private def sortProblem[T](name: String, build: Array[Integer] => T)(sort: T => Any) = ProblemConfig(
    name,
    outputTypes = IS(EVect(EInt)),
    sizeF = {
      case IS(VectValue(vec)) =>
        vec.size
    },
    resourceUsage = {
      case IS(VectValue(vec)) =>
        val arr = vecToJavaIntArray(vec)
        val ds = build(arr)
        BenchmarkSet.measureCost {
          sort(ds)
        }
    },
    saveValueWithName = (value: IS[EValue], name: String) => {
      value match {
        case IS(VectValue(v)) =>
          val str = v.map { case IntValue(j) => j }.mkString("\n")
          val textFileName = s"$name.raw.txt"
          FileInteraction.writeToFile(textFileName)(str)
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
    },
    saveValueWithName = (value: IS[EValue], name: String) => {
      value match {
        case IS(g: GraphValue) =>
          val adjMatrix = translateAdjMatrix(g, lowerWeight, upperWeight)
          val matrix1 = for ((line, i) <- adjMatrix.zipWithIndex) yield {
            line.take(i)++line.drop(i+1)
          }
          val str = matrix1.map(row =>  row.mkString(" ")).mkString("\n")
          val textFileName = s"$name.raw.txt"
          FileInteraction.writeToFile(textFileName)(str)
      }
    }
  )

  def listInsert = simpleProblem("wise.listInsert", patbench.wise.SortedListInsert.build)(patbench.wise.SortedListInsert.insert)
  def heapInsert = simpleProblem("wise.heapInsert", patbench.wise.HeapInsertJDK15.build)(patbench.wise.HeapInsertJDK15.insert)
  def bstSearch = simpleProblem("wise.bstSearch", patbench.wise.BinaryTreeSearch.build)(patbench.wise.BinaryTreeSearch.search)
  def rbSearch = simpleProblem("wise.rbTreeSearch", patbench.wise.RedBlackTreeSearch.build)(patbench.wise.RedBlackTreeSearch.search)
  def quickSort = sortProblem("wise.qsort", patbench.wise.QuickSortJDK15.build)(patbench.wise.QuickSortJDK15.sort)
  def mergeSort = sortProblem("wise.mergeSort", patbench.wise.MergeSortJDK15.build)(patbench.wise.MergeSortJDK15.sort)
  def dijkstra = graphProblem("wise.dijkstra", (n, d) => patbench.wise.Dijkstra.runDijkstra(n, d, 0), 0, 1000)
  def bellmanFord = graphProblem("wise.bellmanFord", (n, d) => patbench.wise.BellmanFord.runBellmanFord(n, d, 0), -1000, 1000)
  def tsp = graphProblem("wise.tsp", patbench.wise.Tsp.runTsp, 0, 1000)

  def allProblems = IS(listInsert, heapInsert, bstSearch, rbSearch, quickSort, mergeSort, dijkstra, bellmanFord, tsp)

  def runWithTimeout(args: Array[String], problems: IS[ProblemConfig], hoursAllowed: Double, evalSize: Int, processNum: Int = 1, maxIteration: Int = 500, baseSeed: Int = 0): Unit = {
    val execConfigTemplate: ExecutionConfig = ExecutionConfig(evalSizePolicy = FixedEvalSize(evalSize), timeLimitInMillis = 10 * 60 * 1000, maxNonIncreaseGen = Some(250))
    val taskNum = problems.length

    SimpleMath.processMap(args, 0 until taskNum, processNum = processNum, mainClass = this) {
      i =>
        val problem = problems(i)
        var timeLeft = (3600 * hoursAllowed).toLong
        var numIters = 0
        while (timeLeft > 0L && numIters < maxIteration) {
          val seed = baseSeed + numIters
          val runnerConfig = RunnerConfig(randomSeed = seed, ioId = seed, useGUI = false, callExitAfterFinish = false)
          try {
            val (timeUsed, _) = TimeTools.measureTime {
              val execConfig = execConfigTemplate.copy(maxFuzzingTimeSec = Some(timeLeft))
              Supernova.standardSupernova.fuzzProblem(problem, runnerConfig, execConfig,
                new Random(seed))
            }
            timeLeft -= (timeUsed / 1000000000)
          } catch {
            case tE: Runner.MaxFuzzingTimeReachedException =>
              println(s"Benchmark $i finished, time limit for this one: ${tE.timeLimitSec}")
              timeLeft = -1
          }
          numIters += 1
        }
    }
  }

  def main(args: Array[String]): Unit = {
//    BenchmarkSet.runExample(100, dijkstra, useGUI=true, size=30)
    runWithTimeout(args, allProblems,
      hoursAllowed = 3,
      evalSize = 30,
      processNum = 9,
      baseSeed = 1000)
  }
}
