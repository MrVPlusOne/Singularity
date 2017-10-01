package patsyn

case class SingleStateInd(seed: IS[Expr], iter: IS[Expr]){
  //todo: add new size measurement with penalty on repeated exprs

  def showAsLinearExpr: String = {
    s"{seed: ${seed.map(Expr.linearShow).mkString("< ", " | ", " >" )} ; " +
      s"iter: ${iter.map(Expr.linearShow).mkString("< ", " | ", " >" )}}"
  }
}

class SingleStateOptimizer() extends EvolutionaryOptimizer[SingleStateInd]{

  def showIndividual(ind: SingleStateInd): String = ind.showAsLinearExpr

  def representationSize(ind: SingleStateInd): Double = {
    (ind.seed ++ ind.iter).map(_.astSize).sum.toDouble
  }

  def individualExprs(ind: SingleStateInd): Seq[Expr] = {
    (ind.seed ++ ind.iter).flatMap(e => Expr.subExprs(e).values)
  }
}
