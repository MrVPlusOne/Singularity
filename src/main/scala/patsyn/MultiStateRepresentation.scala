package patsyn

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

  def allSubExprs: IS[(GlobalCoord, Expr)] = {
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
    (stateTypes.indices.map{ i =>
      val t = stateTypes(i)
      val s = ind.seeds(i)
      val iter = ind.iters(i)
      s"[s$i: $t]{seed: $s ; iter: $iter}"
    } ++ ind.outputs.indices.map{ i =>
      val t = outputTypes(i)
      val out = ind.outputs(i)
      s"[o$i: $t]{output: $out}"
    }).mkString("<", " | ", ">")
  }

  def representationSize(ind: MultiStateInd): Double = {
    ind.totalAstSize.toDouble
  }

  def individualExprs(ind: MultiStateInd): Seq[Expr] = {
    ind.allSubExprs.map(_._2)
  }

  def individualToPattern(ind: MultiStateInd): Stream[IS[EValue]] = {
    val iters = ind.iters
    val initStates = ind.seeds.map(seed => Expr.evaluateWithCheck(seed, IS()))
    val states = Stream.iterate(initStates)(ls => {
            iters.map { iter => Expr.evaluateWithCheck(iter, ls) }
          })
//    println(s"IndividualToPattern: ${showIndividual(ind)}")
    states.map(xs => ind.outputs.map(f => Expr.evaluateWithCheck(f, xs)))
  }

  def sizePenaltyFactor(ind: MultiStateInd): Double = {
    val totalSize = (ind.seeds ++ ind.iters ++ ind.outputs).map(_.astSize).sum
    SimpleMath.gaussianForthOrder(totalSizeTolerance)(totalSize)
  }
}
