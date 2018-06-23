package singularity.benchmarks

import BenchmarkSet._
import singularity._
import StandardSystem._
import patbench.jgrapht
import patbench.jgrapht.alg.StoerWagnerMinimumCut
import patbench.jgrapht.alg.color._
import patbench.jgrapht.alg.cycle._
import patbench.jgrapht.alg.flow._
import patbench.jgrapht.alg.interfaces.{MaximumFlowAlgorithm, MinimumVertexCoverAlgorithm, SpanningTreeAlgorithm, VertexColoringAlgorithm}
import patbench.jgrapht.alg.scoring.{ClosenessCentrality, Coreness}
import patbench.jgrapht.alg.spanning.{BoruvkaMinimumSpanningTree, KruskalMinimumSpanningTree, PrimMinimumSpanningTree}
import patbench.jgrapht.alg.vertexcover.{ClarksonTwoApproxVCImpl, EdgeBasedTwoApproxVCImpl, GreedyVCImpl, RecursiveExactVCImpl}
import patbench.jgrapht.graph._
import singularity.Runner.RunnerConfig

import scala.util.Random

object JGraphTExamples {

  object MamFormat{
    def showDouble(v: Double, precision: Int = 6): String = {
      f"$v%.6e".replace("e+","*10^").replace("e-","*10^-")
    }

    def showAsMamGraph(graph: GraphValue): String = {
      val vertexList = (0 until graph.nodeNum).map(i => s"Labeled[$i, Style[$i,Red]]").mkString("{",",","}")
      val edgeList = graph.edges.map{
        case (from, to, value) => s"""Labeled[$from -> $to,Tooltip["$value"]]"""
      }.mkString("{",",","}")
      s"""Graph[$vertexList, $edgeList]"""
    }
  }

  def handleException[A](default: A)(f: => A): A = {
//    f
    try{
      f
    }catch {
      case _: IllegalArgumentException => default
    }
  }

  def maxFlowTest(name: String,
                  algorithm: DirectedWeightedMultigraph[Integer, DefaultWeightedEdge] => MaximumFlowAlgorithm[Integer, DefaultWeightedEdge]) = ProblemConfig(
    name,
    outputTypes = IS(EGraph(EInt), EInt, EInt),
    resourceUsage = {
      case IS(gv: GraphValue, IntValue(source), IntValue(sink)) =>
        handleException(0.0) {
          val positiveGraph = gv.copy(edges = gv.edges.map{
            case (f,t, IntValue(v)) => (f,t, IntValue(SimpleMath.safeAbs(v)))
            case _ => throw new Exception("wrong edge type")
          })
          val g = TestJGraphT.mkDirectedWeightedMultiGraph(positiveGraph)
          measureCost {
            val solver = algorithm(g)
            solver.getMaximumFlow(source, sink).getFlow()
          }
        }
    },
    saveValueWithName = (values, name) => values match {
      case IS(graphValue: GraphValue, IntValue(i1), IntValue(i2)) =>
        FuzzingTaskProvider.defaultSaveValueWithName(values, name)
        val content =
          s"""
             |$i1
             |$i2
             |${MamFormat.showAsMamGraph(graphValue)}
        """.stripMargin
        FileInteraction.writeToFile(name+"-graph.txt"){content}
    }
  )

  def maxFlow_EdmondsKarp = maxFlowTest("jGraphT.maxFlow.EdmondsKarp",
    g => new EdmondsKarpMFImpl[Integer, DefaultWeightedEdge](g))

  def maxFlow_PushRelabelMFImpl= maxFlowTest("jGraphT.maxFlow.PushRelabelMFImpl",
    g => new PushRelabelMFImpl[Integer, DefaultWeightedEdge](g))


  val maxFlowProblems = IS(
    maxFlow_EdmondsKarp,
    maxFlow_PushRelabelMFImpl
  )

  def spanningTreeTest(name: String,
                       algorithm: DirectedWeightedMultigraph[Integer, DefaultWeightedEdge] =>
                         SpanningTreeAlgorithm[DefaultWeightedEdge]) =
    ProblemConfig(
      name,
      outputTypes = IS(EGraph(EInt)),
      resourceUsage = {
        case IS(gv: GraphValue) =>
          handleException(0.0) {
            val graph = TestJGraphT.mkDirectedWeightedMultiGraph(gv)
            measureCost {
              val solver = algorithm(graph)
              solver.getSpanningTree()
            }
          }
      }
    )

  val spanningTree_prim = spanningTreeTest("jgraphT.spanningTree.Prim",
    g => new PrimMinimumSpanningTree[Integer, DefaultWeightedEdge](g))
  val spanningTree_kruskal = spanningTreeTest("jgraphT.spanningTree.Kruskal",
    g => new KruskalMinimumSpanningTree[Integer, DefaultWeightedEdge](g))
  val spanningTree_boruvka = spanningTreeTest("jgraphT.spanningTree.Boruvka",
    g => new BoruvkaMinimumSpanningTree[Integer, DefaultWeightedEdge](g))

  val spanningTreeProblems = IS(
    spanningTree_prim -> 200,
    spanningTree_kruskal -> 200,
    spanningTree_boruvka -> 200
  )

  def minCutProblem = ProblemConfig(
    "jgraphT.mincut.StoerWagner",
    outputTypes = IS(EGraph(EInt)),
    resourceUsage = {
      case IS(gv: GraphValue) =>
        handleException(0.0) {
          val graph = TestJGraphT.mkSimpleWeightedGraph(gv)
          measureCost {
            val solver = new StoerWagnerMinimumCut[Integer, DefaultWeightedEdge](graph)
            solver.minCutWeight()
          }
        }
    }
  )


  def vertexColoringTest(name: String,
                         algorithm: jgrapht.Graph[Integer, DefaultEdge] => VertexColoringAlgorithm[Integer]) = ProblemConfig(
    name, outputTypes = IS(EGraph(EUnit)),
    resourceUsage = {
      case IS(graphValue: GraphValue) =>
        handleException(0.0){
          val g = TestJGraphT.mkSimpleGraph(graphValue)
          measureCost{
            algorithm(g).getColoring
          }
        }
    }
  )

  def vertexColor_greedy = vertexColoringTest("jGraphT.vertexColor.GreedyColoring",
    g => new GreedyColoring[Integer, DefaultEdge](g)
  )

  def vertexColor_largestDegreeFirst = vertexColoringTest("jGraphT.vertexColor.LargestDegreeFirst",
    g => new LargestDegreeFirstColoring[Integer, DefaultEdge](g)
  )

  def vertexColor_saturationDegree = vertexColoringTest("jGraphT.vertexColor.SaturationDegreeColoring",
    g => new SaturationDegreeColoring[Integer, DefaultEdge](g)
  )

  def vertexColor_smallestDegreeLast= vertexColoringTest("jGraphT.vertexColor.SmallestDegreeLastColoring",
    g => new SmallestDegreeLastColoring[Integer, DefaultEdge](g)
  )

  val coloringProblems = IS(
    vertexColor_greedy,
    vertexColor_largestDegreeFirst,
    vertexColor_saturationDegree,
    vertexColor_smallestDegreeLast
  )

  def toDefaultDirectedGraph(g: GraphValue, directed: Boolean = true) = {
    val graph = if(directed) new DefaultDirectedGraph[Integer, DefaultEdge](
      new ClassBasedEdgeFactory[Integer, DefaultEdge](classOf[DefaultEdge]))
    else new SimpleGraph[Integer, DefaultEdge](classOf[DefaultEdge])

    (0 until g.nodeNum).foreach(i => graph.addVertex(i))
    g.edges.foreach{
      case (from, to, _) => graph.addEdge(from, to)
    }
    graph
  }

  // no anomaly found
  def simpleCyclesProblem(name: String, algorithm: DirectedSimpleCycles[Integer, DefaultEdge]) = {
    ProblemConfig(
      name,
      outputTypes = IS(EGraph(EUnit)),
      resourceUsage = {
        case IS(g: GraphValue) =>
          val graph = toDefaultDirectedGraph(g)
          measureCost{
//            handleException(()){
              algorithm.setGraph(graph)
              algorithm.findSimpleCycles()
//            }
          }
      }
    )
  }

  val hawickJamesSimpleCyclesProblem = simpleCyclesProblem(
    "jGraphT.simpleCycles.hawickJames", new HawickJamesSimpleCycles[Integer, DefaultEdge]()
  )

  val tiernanSimpleCyclesProblem = simpleCyclesProblem(
    "jGraphT.simpleCycles.tiernan", new TiernanSimpleCycles[Integer, DefaultEdge]()
  )

  val tarjanSimpleCyclesProblem = simpleCyclesProblem(
    "jGraphT.simpleCycles.tarjan", new TarjanSimpleCycles[Integer, DefaultEdge]()
  )

  val johnsonSimpleCyclesProblem = simpleCyclesProblem(
    "jGraphT.simpleCycles.johnson", new JohnsonSimpleCycles[Integer, DefaultEdge]()
  )

  val szwarcfiterLauerSimpleCyclesProblem = simpleCyclesProblem(
    "jGraphT.simpleCycles.szwarcfiterLauer", new SzwarcfiterLauerSimpleCycles[Integer, DefaultEdge]()
  )

  val simpleCyclesProblems = IS(hawickJamesSimpleCyclesProblem, tiernanSimpleCyclesProblem, tarjanSimpleCyclesProblem, johnsonSimpleCyclesProblem, szwarcfiterLauerSimpleCyclesProblem)

  def closenessExample = {
    ProblemConfig(
      "jGraphT.closeness",
      outputTypes = IS(EGraph(EUnit)),
      resourceUsage = {
        case IS(g: GraphValue) =>
          val graph = toDefaultDirectedGraph(g)
          measureCost {
            val c = new ClosenessCentrality[Integer, DefaultEdge](graph)
            c.getScores
          }
      }
    )
  }

  def corenessExample = {
    ProblemConfig(
      "jGraphT.coreness",
      outputTypes = IS(EGraph(EUnit)),
      resourceUsage = {
        case IS(g: GraphValue) =>
          val graph = toDefaultDirectedGraph(g, directed = false)
          measureCost {
            val c = new Coreness[Integer, DefaultEdge](graph)
            c.getScores
          }
      }
    )
  }

  def boruvkaMinimumSpanningTreeProblem = {
    ProblemConfig(
      "jGraphT.boruvkaMST",
      outputTypes = IS(EGraph(EInt)),
      resourceUsage = {
        case IS(g: GraphValue) =>
          handleException(0.0){
            val graph = TestJGraphT.mkSimpleWeightedGraph(g)
            measureCost {
              new BoruvkaMinimumSpanningTree[Integer, DefaultWeightedEdge](graph).getSpanningTree
              new KruskalMinimumSpanningTree[Integer, DefaultWeightedEdge](graph).getSpanningTree
            }
          }
      }
    )
  }

  def kruskalMinimumSpanningTreeProblem = {
    ProblemConfig(
      "jGraphT.kruskalMST",
      outputTypes = IS(EGraph(EInt)),
      resourceUsage = {
        case IS(g: GraphValue) =>
          handleException(0.0){
            val graph = TestJGraphT.mkSimpleWeightedGraph(g)
            measureCost {
              new KruskalMinimumSpanningTree[Integer, DefaultWeightedEdge](graph).getSpanningTree
            }
          }
      }
    )
  }

  def primMinimumSpanningTreeProblem = {
    ProblemConfig(
      "jGraphT.primMST",
      outputTypes = IS(EGraph(EInt)),
      resourceUsage = {
        case IS(g: GraphValue) =>
          handleException(0.0){
            val graph = TestJGraphT.mkDirectedWeightedMultiGraph(g)
            measureCost {
              new PrimMinimumSpanningTree[Integer, DefaultWeightedEdge](graph).getSpanningTree
            }
          }
      }
    )
  }

  def vertexCoverProblem(name: String, alg: ()=>MinimumVertexCoverAlgorithm[Integer, DefaultEdge]) = {
    ProblemConfig(
      name,
      outputTypes = IS(EGraph(EUnit)),
      resourceUsage = {
        case IS(g: GraphValue) =>
          handleException(0.0){
            val graph = toDefaultDirectedGraph(g, directed = false)
            measureCost {
              alg().getVertexCover(graph)
            }
          }
      }
    )
  }

  val edgeBased2VC = vertexCoverProblem("EdgeBasedTwoApproxVCImpl", () => new EdgeBasedTwoApproxVCImpl())
  val clarkson2VC = vertexCoverProblem("ClarksonTwoApproxVCImpl", ()=> new ClarksonTwoApproxVCImpl())
  val greedy2VC = vertexCoverProblem("GreedyTwoApproxVCImpl", ()=> new GreedyVCImpl())
  val recursiveExactVC = vertexCoverProblem("RecursiveExactVCImpl", ()=> new RecursiveExactVCImpl())


}
