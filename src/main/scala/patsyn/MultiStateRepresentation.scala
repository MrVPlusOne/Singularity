package patsyn

import patsyn.EvolutionRepresentation.MemoryUsage
import patsyn.MultiStateInd.GlobalCoord

object MultiStateInd {
  type GlobalCoord = (Int, Expr.Coordinate)
}

case class MultiStateInd(exprs: IS[Expr], nStates: Int){
  val seeds: IS[Expr] = exprs.take(nStates)
  val iters: IS[Expr] = (nStates until 2*nStates).map(i => exprs(i))
  val outputs: IS[Expr] = exprs.drop(2*nStates)

  def totalAstSize: Int = {
    exprs.map(_.astSize).sum
  }

  lazy val allSubExprs: IS[(GlobalCoord, Expr)] = {
    exprs.indices.flatMap(i =>
      Expr.subExprs(exprs(i)).map{
        case (c, e) => (i, c) -> e
      }
    )
  }

  def isSeed(id: Int): Boolean = id < nStates

  def update(coord: GlobalCoord, newSubTree: Expr): MultiStateInd = {
    val (i, eCoord) = coord
    val newExpr = Expr.replaceSubExpr(exprs(i), newSubTree, eCoord)
    this.copy(exprs.updated(i, newExpr), nStates)
  }
}


case class MultiStateRepresentation(stateTypes: IS[EType], outputTypes: IS[EType],
                                    totalSizeTolerance: Double, evaluation: PerformanceEvaluation)
  extends EvolutionRepresentation[MultiStateInd] {

  def showIndividual(ind: MultiStateInd): String = {
    (ind.outputs.indices.map { i =>
      val t = outputTypes(i)
      val out = ind.outputs(i)
      s"[O$i: $t]{ $out }"
    } ++ stateTypes.indices.map { i =>
      val t = stateTypes(i)
      val s = ind.seeds(i)
      val iter = ind.iters(i)
      s"[S$i: $t]{ seed: $s ; iter: $iter }"
    }).mkString("< ", " | ", " >")
  }

  def printIndividualMultiLine(println: String => Unit)(ind: MultiStateInd): Unit = {
    ind.outputs.indices.foreach { i =>
      val t = outputTypes(i)
      val out = ind.outputs(i)
      println(s"[O$i: $t] -> $out")
    }
    println("^^^")
    stateTypes.indices.foreach { i =>
      val t = stateTypes(i)
      val s = ind.seeds(i)
      val iter = ind.iters(i)
      val seedValue = Expr.evaluateWithCheck(s, IS())
      println(s"[S$i: $t]{ seed: $seedValue ; iter: $iter; seedExpr: $s }")
    }
  }

  def representationSize(ind: MultiStateInd): Double = {
    ind.totalAstSize.toDouble
  }

  def individualExprs(ind: MultiStateInd): Seq[Expr] = {
    ind.allSubExprs.map(_._2)
  }

  def individualToPattern(ind: MultiStateInd): Stream[(MemoryUsage, IS[EValue])] = {
    val iters = ind.iters
    val initStates = ind.seeds.map(seed => Expr.evaluateWithCheck(seed, IS()))
    val states = Stream.iterate(initStates)(ls => {
            iters.map { iter => Expr.evaluateWithCheck(iter, ls) }
          })
    states.map(xs => {
      val totalSize = xs.map(_.size).sum
      MemoryUsage(totalSize) -> ind.outputs.map(f => Expr.evaluateWithCheck(f, xs))
    })
  }

  def sizePenaltyFactor(ind: MultiStateInd): Double = {
    val totalSize = (ind.seeds ++ ind.iters ++ ind.outputs).map(_.astSize).sum
    SimpleMath.gaussianForthOrder(totalSizeTolerance)(totalSize)
  }
}
