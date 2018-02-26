package benchmarks

import java.util.Random

import patsyn.Runner.RunnerConfig
import patsyn.StandardSystem._
import patsyn._

object TextbookExamples {

  private def vecToJavaIntArray(vec: Vector[EValue]): Array[Integer] =
    vec.map { case IntValue(i) => new Integer(i) }.toArray

  private def sortingProblem(name: String, func: Array[Comparable[_]] => Any) = ProblemConfig(
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
            // Array is invariant in Scala so an explicit cast is needed here to  pass Array[Integer] to Array[Comparable].
            func(arr.asInstanceOf[Array[Comparable[_]]])
          }
      }
    )

  private def searchingProblem[T](name: String, ctor: Unit => T)(put: T => (Integer, Integer) => Unit, get: T => Integer => Integer) = ProblemConfig(
      name,
      outputTypes = IS(EVect(EInt), EInt),
      sizeF = {
        case IS(vec, _) =>
          vec.memoryUsage.toInt
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
      case IS(u, v) =>
        u.memoryUsage.toInt + v.memoryUsage.toInt
    },
    resourceUsage = {
      case IS(VectValue(needle), VectValue(haystack)) =>
        val needleStr = FuzzingTaskProvider.vectIntToString(needle, 8)
        val haystackStr = FuzzingTaskProvider.vectIntToString(haystack, 8)

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
      case IS(g) =>
        g.memoryUsage.toInt
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
  def insertSort = sortingProblem("textbook.insertSort", patbench.textbook.Insertion.sort)
  def insertSortOpt = sortingProblem("textbook.insertSortOpt", patbench.textbook.InsertionX.sort)
  def insertSortBinary = sortingProblem("textbook.insertSortBinary", patbench.textbook.BinaryInsertion.sort)
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
  def rkStr = stringProblem("textbook.rabinKarp", s => new patbench.textbook.RabinKarp(s))(_.search)
  def nfaStr = stringProblem("textbook.nfa", s => new patbench.textbook.NFA(s))(_.recognizes)

  // Graph problems
  def dfs = graphProblem("textbook.dfs", translateGraph)(g => new patbench.textbook.DepthFirstPaths(g, 0))
  def bfs = graphProblem("textbook.bfs", translateGraph)(g => new patbench.textbook.BreadthFirstPaths(g, 0))
  def altPathBiMatch = graphProblem("textbook.altPathBiMatch", translateGraph)(g => new patbench.textbook.BipartiteMatching(g))
  def hkBiMatch = graphProblem("textbook.hopcroftKarpBiMatch", translateGraph)(g => new patbench.textbook.HopcroftKarp(g))
  def prim = graphProblem("textbook.prim", translateWeightedGraph)(g => new patbench.textbook.PrimMST(g))
  def kruskal = graphProblem("textbook.kruskal", translateWeightedGraph)(g => new patbench.textbook.KruskalMST(g))
  def boruvka = graphProblem("textbook.boruvka", translateWeightedGraph)(g => new patbench.textbook.BoruvkaMST(g))
  def dijkstra = graphProblem("textbook.dijkstra", translateWeightedDigraph)(g => new patbench.textbook.DijkstraSP(g, 0))
  def bellmanFord = graphProblem("textbook.bellmanFord", translateWeightedDigraph)(g => new patbench.textbook.BellmanFordSP(g, 0))
  def fordFulkerson = graphProblem("textbook.fordFulkerson", translateFlowNetwork)(g => new patbench.textbook.FordFulkerson(g, 0, g.V() - 1))

  // Miscellaneous
  def grahamScan = ProblemConfig(
    "textbook.grahamScan",
    outputTypes = IS(EVect(EPair(EInt, EInt))),
    sizeF = {
      case IS(v) =>
        v.memoryUsage.toInt
    },
    resourceUsage = {
      case IS(VectValue(vec)) =>
        val points = vec.map { value: EValue =>
          val (xValue, yValue) = value.asInstanceOf[PairValue].value
          new patbench.textbook.Point2D(xValue.asInstanceOf[IntValue].value, yValue.asInstanceOf[IntValue].value)
        }.toArray
        BenchmarkSet.handleRuntimeException(0L) {
          BenchmarkSet.measureCost {
            new patbench.textbook.GrahamScan(points)
          }
        }
    }
  )

  val allProblems = IS(insertSort, insertSortOpt, insertSortBinary, selectSort, shellSort, mergeSortTD, mergeSortBU, mergeSortOpt, quickSort, quickSort3Way, quickSortOpt, quickSort3WayOpt, heapSort, seqSearch, binSearch, bstSearch, rbSearch, sepChainHash, linProbeHash, kmpStr, bmStr, rkStr, nfaStr, dfs, bfs, altPathBiMatch, hkBiMatch, prim, kruskal, boruvka, dijkstra, bellmanFord, fordFulkerson, grahamScan)

  def runExample(seed: Int, useGUI: Boolean): Unit = {
    val rand = new Random(seed)
    Supernova.standardSupernova.fuzzProblem(
      dijkstra,
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
