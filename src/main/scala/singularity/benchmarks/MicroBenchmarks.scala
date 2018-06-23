package singularity.benchmarks

import singularity.StandardSystem._
import singularity._

object MicroBenchmarks {

  import patbench.microbench._

  private def vecToJavaIntArray(vec: Vector[EValue]) =
    vec.map { case IntValue(i) => new Integer(i) }.toArray

  def insertionSort = ProblemConfig(
    "micro.isort",
    outputTypes = IS(EVect(EInt)),
    sizeF = {
      case IS(v) =>
        v.memoryUsage.toInt
    },
    resourceUsage = {
      case IS(VectValue(vec)) =>
        val arr = vecToJavaIntArray(vec)
        BenchmarkSet.measureCost {
          isort.InsertionSort.sort(arr)
        }
    }
  )

  def bubbleSort = ProblemConfig(
    "micro.bsort",
    outputTypes = IS(EVect(EInt)),
    sizeF = {
      case IS(v) =>
        v.memoryUsage.toInt
    },
    resourceUsage = {
      case IS(VectValue(vec)) =>
        val arr = vecToJavaIntArray(vec)
        BenchmarkSet.measureCost {
          bsort.BubbleSort.sort(arr)
        }
    }
  )

  def quickSortPivotFirst = ProblemConfig(
    "micro.qsortFirst",
    outputTypes = IS(EVect(EInt)),
    sizeF = {
      case IS(v) =>
        v.memoryUsage.toInt
    },
    resourceUsage = {
      case IS(VectValue(vec)) =>
        val arr = vecToJavaIntArray(vec)
        BenchmarkSet.measureCost {
          qsort.QuickSort.sortPivotFirst(arr)
        }
    }
  )

  def quickSortPivotMiddle = ProblemConfig(
    "micro.qsortMiddle",
    outputTypes = IS(EVect(EInt)),
    sizeF = {
      case IS(v) =>
        v.memoryUsage.toInt
    },
    resourceUsage = {
      case IS(VectValue(vec)) =>
        val arr = vecToJavaIntArray(vec)
        BenchmarkSet.measureCost {
          qsort.QuickSort.sortPivotMiddle(arr)
        }
    }
  )

  def treeSort = ProblemConfig(
    "micro.treesort",
    outputTypes = IS(EVect(EInt)),
    sizeF = {
      case IS(v) =>
        v.memoryUsage.toInt
    },
    resourceUsage = {
      case IS(VectValue(vec)) =>
        val arr = vecToJavaIntArray(vec)
        BenchmarkSet.measureCost {
          tsort.TreeSort.sortArray(arr)
        }
    }
  )

  def shellSortShell = ProblemConfig(
    "micro.ssortShell",
    outputTypes = IS(EVect(EInt)),
    sizeF = {
      case IS(v) =>
        v.memoryUsage.toInt
    },
    resourceUsage = {
      case IS(VectValue(vec)) =>
        val arr = vecToJavaIntArray(vec)
        BenchmarkSet.measureCost {
          ssort.ShellSort.sortShellSeq(arr)
        }
    }
  )

  def shellSortFrankLazarus = ProblemConfig(
    "micro.ssortFrankLazarus",
    outputTypes = IS(EVect(EInt)),
    sizeF = {
      case IS(v) =>
        v.memoryUsage.toInt
    },
    resourceUsage = {
      case IS(VectValue(vec)) =>
        val arr = vecToJavaIntArray(vec)
        BenchmarkSet.measureCost {
          ssort.ShellSort.sortFrankLazarusSeq(arr)
        }
    }
  )

  def shellSortHibbard = ProblemConfig(
    "micro.ssortHibbard",
    outputTypes = IS(EVect(EInt)),
    sizeF = {
      case IS(v) =>
        v.memoryUsage.toInt
    },
    resourceUsage = {
      case IS(VectValue(vec)) =>
        val arr = vecToJavaIntArray(vec)
        BenchmarkSet.measureCost {
          ssort.ShellSort.sortHibbardSeq(arr)
        }
    }
  )

  def shellSortSedgewick = ProblemConfig(
    "micro.ssortSedgewick",
    outputTypes = IS(EVect(EInt)),
    sizeF = {
      case IS(v) =>
        v.memoryUsage.toInt
    },
    resourceUsage = {
      case IS(VectValue(vec)) =>
        val arr = vecToJavaIntArray(vec)
        BenchmarkSet.measureCost {
          ssort.ShellSort.sortSedgewickSeq(arr)
        }
    }
  )

  def stringSearch = ProblemConfig(
    "micro.bmh",
    outputTypes = IS(EVect(EInt), EVect(EInt)),
    sizeF = {
      case IS(u, v) =>
        u.memoryUsage.toInt + v.memoryUsage.toInt
    },
    resourceUsage = {
      case IS(VectValue(needle), VectValue(haystack)) =>
        val needleStr = ExampleAlgorithms.vectIntToString(needle, 8)
        val haystackStr = ExampleAlgorithms.vectIntToString(haystack, 8)
        BenchmarkSet.handleIllegalArgumentException(0L) {
          BenchmarkSet.measureCost {
            bmh.BoyerMooreHorspool.search(needleStr, haystackStr)
          }
        }
    }
  )

  def giftWrap = ProblemConfig(
    "micro.giftwrap",
    outputTypes = IS(EVect(EPair(EInt, EInt))),
    sizeF = {
      case IS(v) =>
        v.memoryUsage.toInt
    },
    resourceUsage = {
      case IS(VectValue(vec)) =>
        val points = vec.map {
          case PairValue((IntValue(x), IntValue(y))) =>
            new geometry.Point(x, y)
        }.toArray
        BenchmarkSet.measureCost {
          geometry.GiftWrapping.convexHullArray(points)
        }
    }
  )

  def graphValueToGraph(graphValue: GraphValue) = {
    val nodeArray = (0 until graphValue.nodeNum).map{ i => new Integer(i) }.toArray
    val edgeArray = graphValue.edges.map {
      case (src, dst, weightValue) =>
        new graph.Edge[Integer](src, dst, weightValue.asInstanceOf[IntValue].value)
    }.toArray
    graph.Graph.createFromArrays(nodeArray, edgeArray)
  }

  def bellmanFord = ProblemConfig(
    "micro.bellmanford",
    outputTypes = IS(EInt, EGraph(EInt)),
    sizeF = {
      case IS(_, graph) =>
        graph.asInstanceOf[GraphValue].nodeNum
    },
    resourceUsage = {
      case IS(IntValue(s), graphValue: GraphValue) =>
        val g = graphValueToGraph(graphValue)
        val sourceNode = SimpleMath.wrapInRange(s, graphValue.nodeNum)
        BenchmarkSet.measureCost {
          graph.BellmanFord.runIntGraph(g, sourceNode)
        }
    }
  )

  def kruskal = ProblemConfig(
    "micro.kruskal",
    outputTypes = IS(EGraph(EInt)),
    sizeF = {
      case IS(graph) =>
        graph.asInstanceOf[GraphValue].nodeNum
    },
    resourceUsage = {
      case IS(graphValue: GraphValue) =>
        val g = graphValueToGraph(graphValue)
        BenchmarkSet.measureCost {
          graph.Kruskal.run(g)
        }
    }
  )

  def allProblems: IS[ProblemConfig] = IS(insertionSort, bubbleSort, quickSortPivotFirst, quickSortPivotMiddle, treeSort, shellSortShell, shellSortFrankLazarus, shellSortHibbard, stringSearch, giftWrap, bellmanFord, kruskal)

//  def main(args: Array[String]): Unit = {
//    BenchmarkSet.runExample(2, bellmanFord, useGUI = true, size = 100)

//    val numPerExample = 40
//    val shift = 100
//    SimpleMath.processMap(args,
//          0 until allProblems.length*numPerExample, processNum = 10,
//          mainClass = this){
//          i =>
//            BenchmarkSet.runExample(i+shift, allProblems(i/numPerExample), useGUI = false, size = 100)
//        }
//  }
}
