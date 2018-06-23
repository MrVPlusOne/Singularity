package singularity.benchmarks

import singularity.measure.TimeTools
import singularity.Runner.RunnerConfig
import singularity.StandardSystem._
import singularity._

import scala.util.Random

object TextbookExamples {

  private def vecToJavaIntArray(vec: Vector[EValue]): Array[Integer] =
    vec.map { case IntValue(i) => new Integer(i) }.toArray

  private def sortingProblem(name: String, func: Array[Comparable[_]] => Any) = ProblemConfig(
      name,
      outputTypes = IS(EVect(EInt)),
      sizeF = {
        case IS(VectValue(v)) =>
          v.value.length
      },
      resourceUsage = {
        case IS(VectValue(vec)) =>
          val arr = vecToJavaIntArray(vec)
          BenchmarkSet.measureCost {
            // Array is invariant in Scala so an explicit cast is needed here to pass Array[Integer] to Array[Comparable].
            func(arr.asInstanceOf[Array[Comparable[_]]])
          }
      }
    )

  private def insertionProblem[T](name: String, ctor: Unit => T)(put: T => (Integer, Integer) => Unit) = ProblemConfig(
    name,
    outputTypes = IS(EVect(EInt)),
    sizeF = {
      case IS(VectValue(v)) =>
        v.value.length
    },
    resourceUsage = {
      case IS(VectValue(vec)) =>
        val arr = vecToJavaIntArray(vec)
        val alg = ctor ()
        BenchmarkSet.measureCost {
          for (e <- arr)
            put(alg)(e, e)
        }
    }
  )

  private def searchingProblem[T](name: String, ctor: Unit => T)(put: T => (Integer, Integer) => Unit, get: T => Integer => Integer) = ProblemConfig(
      name,
      outputTypes = IS(EVect(EInt), EInt),
      sizeF = {
        case IS(VectValue(v), _) =>
          v.value.length
      },
      resourceUsage = {
        case IS(VectValue(vec), IntValue(i)) =>
          val arr = vecToJavaIntArray(vec)
          val alg = ctor ()
          for (e <- arr)
            put(alg)(e, e)
          BenchmarkSet.measureCost {
            get(alg)(new Integer(i))
          }
      }
    )

  def stringProblem[T](name: String, ctor: String => T)(search: T => String => Any) = ProblemConfig(
    name,
    outputTypes = IS(EVect(EInt), EVect(EInt)),
    sizeF = {
      case IS(VectValue(u), VectValue(v)) =>
        u.value.length + v.value.length
    },
    resourceUsage = {
      case IS(VectValue(needle), VectValue(haystack)) =>
        val needleStr = ExampleAlgorithms.vectIntToString(needle, 8)
        val haystackStr = ExampleAlgorithms.vectIntToString(haystack, 8)

        BenchmarkSet.handleRuntimeException(0L) {
          val searcher = ctor(needleStr)
          BenchmarkSet.measureCost {
            search(searcher)(haystackStr)
          }
        }
    }
  )

  private def translateGraph(graphValue: GraphValue) = {
    val g = new patbench.textbook.Graph(graphValue.nodeNum)
    graphValue.edges.foreach {
      case (src: Int, dst: Int, _) =>
        g.addEdge(src, dst)
    }
    g
  }

  private def translateWeightedGraph(graphValue: GraphValue) = {
    val g = new patbench.textbook.EdgeWeightedGraph(graphValue.nodeNum)
    graphValue.edges.foreach {
      case (src: Int, dst: Int, weightValue: EValue) =>
        g.addEdge(new patbench.textbook.Edge(src, dst, weightValue.asInstanceOf[IntValue].value))
    }
    g
  }

  private def translateWeightedDigraph(graphValue: GraphValue) = {
    val g = new patbench.textbook.EdgeWeightedDigraph(graphValue.nodeNum)
    graphValue.edges.foreach {
      case (src: Int, dst: Int, weightValue: EValue) =>
        g.addEdge(new patbench.textbook.DirectedEdge(src, dst, weightValue.asInstanceOf[IntValue].value))
    }
    g
  }

  private def translateFlowNetwork(graphValue: GraphValue) = {
    val g = new patbench.textbook.FlowNetwork(graphValue.nodeNum)
    graphValue.edges.foreach {
      case (src: Int, dst: Int, weightValue: EValue) =>
        val capacity = SimpleMath.safeAbs(weightValue.asInstanceOf[IntValue].value)
        g.addEdge(new patbench.textbook.FlowEdge(src, dst, capacity))
    }
    g
  }

  def graphProblem[G, A](name: String, translator: GraphValue => G)(ctor: G => A) = ProblemConfig(
    name,
    outputTypes = IS(EGraph(EInt)),
    sizeF = {
      case IS(GraphValue(nodeNum, edges)) =>
        nodeNum + edges.length
    },
    resourceUsage = {
      case IS(g: GraphValue) =>
        val graph = translator(g)
        BenchmarkSet.handleRuntimeException(0L) {
          BenchmarkSet.measureCost {
            ctor(graph)
          }
        }
    }
  )

  // Sorting problems
  def insertSortOpt = sortingProblem("textbook.insertSortOpt", patbench.textbook.InsertionX.sort)
  def selectSort = sortingProblem("textbook.selectSort", patbench.textbook.Selection.sort)
  def shellSort = sortingProblem("textbook.shellSort", patbench.textbook.Shell.sort)
  def mergeSortTD = sortingProblem("textbook.mergeSortTopDown", patbench.textbook.Merge.sort)
  def mergeSortBU = sortingProblem("textbook.mergeSortBottomUp", patbench.textbook.MergeBU.sort)
  def mergeSortOpt = sortingProblem("textbook.mergeSortX", patbench.textbook.MergeX.sort)
  def quickSort = sortingProblem("textbook.quickSort", patbench.textbook.Quick.sort)
  def quickSort3Way = sortingProblem("textbook.quickSort3Way", patbench.textbook.Quick3way.sort)
  def quickSortOpt = sortingProblem("textbook.quickSortOpt", patbench.textbook.QuickX.sort)
  def quickSort3WayOpt = sortingProblem("textbook.quickSort3WayOpt", patbench.textbook.QuickBentleyMcIlroy.sort)
  def heapSort = sortingProblem("textbook.heapSort", patbench.textbook.Heap.sort)

  // Insertion problems
  def seqInsert = insertionProblem("textbook.seqInsert", _ => new patbench.textbook.SequentialSearchST[Integer, Integer])(_.put)
  def binInsert = insertionProblem("textbook.binInsert", _ => new patbench.textbook.BinarySearchST[Integer, Integer])(_.put)
  def bstInsert = insertionProblem("textbook.bstInsert", _ => new patbench.textbook.BST[Integer, Integer])(_.put)
  def rbInsert = insertionProblem("textbook.rbInsert", _ => new patbench.textbook.RedBlackBST[Integer, Integer])(_.put)
  def sepChainInsert = insertionProblem("textbook.sepChainInsert", _ => new patbench.textbook.SeparateChainingHashST[Integer, Integer])(_.put)
  def linProbeInsert = insertionProblem("textbook.linProbeInsert", _ => new patbench.textbook.LinearProbingHashST[Integer, Integer])(_.put)

  // Searching problems
  def seqSearch = searchingProblem("textbook.seqSearch", _ => new patbench.textbook.SequentialSearchST[Integer, Integer])(_.put, _.get)
  def binSearch = searchingProblem("textbook.binSearch", _ => new patbench.textbook.BinarySearchST[Integer, Integer])(_.put, _.get)
  def bstSearch = searchingProblem("textbook.bstSearch", _ => new patbench.textbook.BST[Integer, Integer])(_.put, _.get)
  def rbSearch = searchingProblem("textbook.rbSearch", _ => new patbench.textbook.RedBlackBST[Integer, Integer])(_.put, _.get)
  def sepChainHash = searchingProblem("textbook.sepChainHash", _ => new patbench.textbook.SeparateChainingHashST[Integer, Integer])(_.put, _.get)
  def linProbeHash = searchingProblem("textbook.linProbeHash", _ => new patbench.textbook.LinearProbingHashST[Integer, Integer])(_.put, _.get)

  // String problems
  def kmpStr = stringProblem("textbook.kmp", s => new patbench.textbook.KMP(s))(_.search)
  def bmStr = stringProblem("textbook.booyerMoore", s => new patbench.textbook.BoyerMoore(s))(_.search)
  def nfaStr = stringProblem("textbook.nfa", s => new patbench.textbook.NFA(s))(_.recognizes)

  // Graph problems
  def altPathBiMatch = graphProblem("textbook.altPathBiMatch", translateGraph)(g => new patbench.textbook.BipartiteMatching(g))
  def hkBiMatch = graphProblem("textbook.hopcroftKarpBiMatch", translateGraph)(g => new patbench.textbook.HopcroftKarp(g))
  def prim = graphProblem("textbook.prim", translateWeightedGraph)(g => new patbench.textbook.PrimMST(g))
  def kruskal = graphProblem("textbook.kruskal", translateWeightedGraph)(g => new patbench.textbook.KruskalMST(g))
  def boruvka = graphProblem("textbook.boruvka", translateWeightedGraph)(g => new patbench.textbook.BoruvkaMST(g))
  def dijkstra = graphProblem("textbook.dijkstra", translateWeightedDigraph)(g => new patbench.textbook.DijkstraSP(g, 0))
  def bellmanFord = graphProblem("textbook.bellmanFord", translateWeightedDigraph)(g => new patbench.textbook.BellmanFordSP(g, 0))
  def fordFulkerson = graphProblem("textbook.fordFulkerson", translateFlowNetwork)(g => new patbench.textbook.FordFulkerson(g, 0, g.V() - 1))

  val allProblems = IS(sepChainHash, linProbeHash, kmpStr, bmStr, nfaStr, altPathBiMatch, hkBiMatch, prim, kruskal, boruvka, dijkstra, bellmanFord, fordFulkerson, quickSort3Way, quickSort3WayOpt, shellSort, mergeSortTD, mergeSortBU, mergeSortOpt, quickSort, quickSortOpt, heapSort, seqSearch, binSearch, bstSearch, rbSearch, insertSortOpt, selectSort, seqInsert, binInsert, bstInsert, rbInsert, sepChainInsert, linProbeInsert)

  def runExample(seed: Int, useGUI: Boolean): Unit = {
    val rand = new java.util.Random(seed)
    Supernova.standardSupernova.fuzzProblem(
      seqSearch,
      RunnerConfig().copy(randomSeed = seed, ioId = seed, useGUI = useGUI),
      ExecutionConfig(evalSizePolicy = FixedEvalSize(250), timeLimitInMillis = 10000), rand,
      overrideGpConfig = config => config.copy(exprCostPenaltyBase = 0.95)
    )
  }

  def runWithTimeout(args: Array[String], problems: IS[ProblemConfig], hoursAllowed: Double, evalSize: Int, processNum: Int = 1, maxIteration: Int = 500, baseSeed: Int = 0): Unit = {
    val execConfigTemplate: ExecutionConfig = ExecutionConfig(evalSizePolicy = FixedEvalSize(evalSize), timeLimitInMillis = 10 * 60 * 1000)
    val taskNum = problems.length

    SimpleMath.processMap(args, 0 until taskNum, processNum = processNum, mainClass = this) {
      i =>
        val problem = problems(i)
        var timeLeft = (3600 * hoursAllowed).toLong
        var numIters = 0
        while (timeLeft > 0L && numIters < maxIteration) {
          val seed = baseSeed + numIters
          val runnerConfig = RunnerConfig(randomSeed = seed, ioId = seed, useGUI = true, callExitAfterFinish = false)
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
    runWithTimeout(args, allProblems,
      hoursAllowed = 3.0,
      evalSize = 250,
      processNum = 8
    )
  }

}
