package patsyn

import visual.PatternPlot

object Playground {
  import StandardSystem._
  import IntComponents._
  import VectComponents._

  val growEdge = GraphComponents.growEdge.concretize(IS(EInt))
  val addNode = GraphComponents.addNode.concretize(IS(EInt))
  val emptyGraph = GraphComponents.emptyGraph.concretize(IS(EInt))
  val bridgeEdge = GraphComponents.bridgeEdge.concretize(IS(EInt))

  implicit def intConst(i: Int): EConst = EConst(EInt, i)

  val seeds: IS[Expr] = IS(
    2,
    bridgeEdge(addNode(addNode(emptyGraph())), 1, 0, 1)
  )

  val arg0 = EArg(0, EInt)
  val arg1 = EArg(1, EGraph(EInt))

  val iters: IS[Expr] = IS(
    inc(arg0),
    bridgeEdge(growEdge(arg1, dec(arg0), Int.MaxValue), arg0, 0, 1)
  )

  val out: IS[Expr] = IS(1, 0, arg1)

  val ind = MultiStateInd(seeds++iters++out, nStates = 2)

  def main(args: Array[String]): Unit = {
//    MultiStateRepresentation.individualToPattern(ind).take(10).foreach{x => println(x._2)}

//    PatternPlot.showResourceUsageChart(FuzzingTaskProvider.dinicExample, ind, 10000, pointDensity = 15)

    println{
//      EVect(EInt).isInstanceOf[Product]
      EInt.isInstanceOf[Product]
    }
  }

}
