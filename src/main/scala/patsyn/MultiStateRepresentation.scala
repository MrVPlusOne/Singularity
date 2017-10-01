package patsyn


case class MultiStateInd(seeds: IS[Expr], iters: IS[Expr], outputs: IS[Expr]){
  def totalAstSize: Int = {
    (seeds ++ iters ++ outputs).map(_.astSize).sum
  }

  def allSubExprs: IndexedSeq[Expr] = {
    (seeds ++ iters ++ outputs).flatMap(e => Expr.subExprs(e).values)
  }
}


class MultiStateRepresentation(stateTypes: IS[EType], outputTypes: IS[EType])
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
    ind.allSubExprs
  }
}
