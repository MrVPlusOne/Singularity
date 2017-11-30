package patsyn

import visual.PatternPlot

object GraphSample {
  import StandardSystem._
  import IntComponents._

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
}

object HashSample {
  import StandardSystem._
  import IntComponents._

  implicit def intConst(i: Int): EConst = EConst(EInt, i)
  implicit def vectConst(v: IS[Int]): EConst = EConst(EVect(EInt), VectValue(v.map(i => IntValue(i)).toVector))

  val append = VectComponents.append.concretize(IS(EInt))
  val appendVec = VectComponents.append.concretize(IS(EVect(EInt)))
  val access = VectComponents.access.concretize(IS(EInt))

  val seeds: IS[Expr] = IS(
    vectConst(IS(0, 310000)),
    EConst(EVect(EVect(EInt)), VectValue(Vector()))
  )

  val arg0 = EArg(0, EVect(EInt))
  val arg1 = EArg(1, EVect(EVect(EInt)))
  val iters: IS[Expr] = IS(
    append(append(vectConst(IS()), inc(access(arg0, 0, 0))), minus(access(arg0, 1, 0), 31)),
    appendVec(arg1, arg0)
  )

  val out: IS[Expr] = IS(arg1)
  val ind = MultiStateInd(seeds++iters++out, nStates = 2)
}

object Playground {


  def main(args: Array[String]): Unit = {
//    MultiStateRepresentation.individualToPattern(HashSample.ind).take(10).foreach{x => println(x._2(0))}
//    MultiStateRepresentation.individualToPattern(HashSample.ind).take(20).foreach{x => println(FuzzingTaskProvider
//      .hashCollisionExample(HashFunc.java).squareMetric(x._2(0).asInstanceOf[StandardSystem.VectValue].value))}

    PatternPlot.showResourceUsageChart(FuzzingTaskProvider.javaHashCollisionExample, HashSample.ind, 4000,
      pointDensity = 15)
  }

}
