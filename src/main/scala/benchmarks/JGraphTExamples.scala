package benchmarks

import BenchmarkSet._
import patsyn._
import StandardSystem._
import patbench.jgrapht
import patbench.jgrapht.alg.color._
import patbench.jgrapht.alg.flow._
import patbench.jgrapht.alg.interfaces.{MaximumFlowAlgorithm, VertexColoringAlgorithm}
import patbench.jgrapht.graph.{DefaultEdge, DefaultWeightedEdge, DirectedWeightedMultigraph, SimpleGraph}
import patsyn.Runner.RunnerConfig

import scala.util.Random

object JGraphTExamples {

  def handleException[A](default: A)(f: => A): A = {
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
          val g = TestJGraphT.mkWeightedGraph(gv)
          measureCost {
            val solver = algorithm(g)
            solver.getMaximumFlow(source, sink).getFlow()
          }
        }
    }
  )

  def maxFlow_EdmondsKarp = maxFlowTest("jGraphT.maxFlow.EdmondsKarp",
    g => new EdmondsKarpMFImpl[Integer, DefaultWeightedEdge](g))

  def maxFlow_PushRelabelMFImpl= maxFlowTest("jGraphT.maxFlow.PushRelabelMFImpl",
    g => new PushRelabelMFImpl[Integer, DefaultWeightedEdge](g))

  def maxFlow_GusfieldEquivalentFlowTree = maxFlowTest("jGraphT.maxFlow.GusfieldEquivalentFlowTree",
    g => new GusfieldEquivalentFlowTree[Integer, DefaultWeightedEdge](g))

  def maxFlow_GusfieldGomoryHuCutTree = maxFlowTest("jGraphT.maxFlow.GusfieldGomoryHuCutTree",
    g => new GusfieldEquivalentFlowTree[Integer, DefaultWeightedEdge](g))

  val maxFlowProblems = IS(
    maxFlow_EdmondsKarp,
    maxFlow_PushRelabelMFImpl,
    maxFlow_GusfieldEquivalentFlowTree,
    maxFlow_GusfieldGomoryHuCutTree
  )


  def vertexColoringTest(name: String,
                         algorithm: SimpleGraph[Integer, DefaultEdge] => VertexColoringAlgorithm[Integer]) = ProblemConfig(
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


  def runExample(seed: Int, problemConfig: ProblemConfig, useGUI: Boolean): Unit = {
    val rand = new Random(seed)
    Supernova.fuzzProblem(
      problemConfig,
      RunnerConfig().copy(randomSeed = seed, ioId = seed, useGUI = useGUI),
      ExecutionConfig(evalSizePolicy = FixedEvalSize(1200)), rand)
  }

  def main(args: Array[String]): Unit = {

    val problems = maxFlowProblems ++ coloringProblems

//    SimpleMath.processMap(args,
//      0 until 8, processNum = 8,
//      mainClass = this){
//      i => runExample(i, problems(i), useGUI = true)
//    }
      runExample(3, problems(3), useGUI = true)

    //    testAverage()
  }
}
