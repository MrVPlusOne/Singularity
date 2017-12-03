package patsyn

import patsyn.EvolutionRepresentation.MemoryUsage

case class SingleStateInd(seed: IS[Expr], iter: IS[Expr]){
  //todo: add new size measurement with penalty on repeated exprs

  def showAsLinearExpr: String = {
    s"{seed: ${seed.map(Expr.linearShow).mkString("< ", " | ", " >" )} ; " +
      s"iter: ${iter.map(Expr.linearShow).mkString("< ", " | ", " >" )}}"
  }
}

case class SingleStateRepresentation(seedSizeTolerance: Int, iterSizeTolerance: Int,
                                     evaluation: PerformanceEvaluation) extends EvolutionRepresentation[SingleStateInd]{

  def showIndividual(ind: SingleStateInd): String = ind.showAsLinearExpr

  def representationSize(ind: SingleStateInd): Double = {
    (ind.seed ++ ind.iter).map(_.astSize).sum.toDouble
  }

  def individualExprs(ind: SingleStateInd): Seq[Expr] = {
    (ind.seed ++ ind.iter).flatMap(e => Expr.subExprs(e).values)
  }

  def individualToPattern(ind: SingleStateInd): Stream[(MemoryUsage, IS[EValue])] = {
    val seeds = ind.seed
    val iters = ind.iter
    val seedValues = seeds.map(seed => Expr.evaluateWithCheck(seed, IS()))
    val s = Stream.iterate(seedValues)(ls => {
      iters.map { iter => Expr.evaluateWithCheck(iter, ls) }
    })

    s.map(ys => {
      MemoryUsage(ys.map(_.memoryUsage).sum) -> ys
    })
  }

  def sizePenaltyFactor(ind: SingleStateInd): Double = {
    val seeds = ind.seed
    val iters = ind.iter
    seeds.map(s => SimpleMath.gaussianForthOrder(seedSizeTolerance)(s.astSize)).product *
      iters.map(iter => SimpleMath.gaussianForthOrder(iterSizeTolerance)(iter.astSize)).product
  }

  def isTooLarge(ind: SingleStateInd): Boolean = {
    (ind.seed ++ ind.iter).map(_.astSize).sum > iterSizeTolerance + seedSizeTolerance
  }
}
