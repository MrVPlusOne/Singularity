package singularity


import singularity.EvolutionRepresentation._

/** Provides utility operations needed for applying Genetic Programming to a type of `Individual` */
trait EvolutionRepresentation[Individual] {
  def showIndividual(ind: Individual): String
  def representationSize(ind: Individual): Double

  /** returns all [[Expr]]s of an individual */
  def individualExprs(ind: Individual): Seq[Expr]

  def individualToPattern(ind: Individual): Stream[(MemoryUsage, IS[EValue])]

  def sizePenaltyFactor(ind: Individual): Double

  /** returns true if an individual is considered to be too large to be useful during GP */
  def isTooLarge(ind: Individual): Boolean

  def evaluation: PerformanceEvaluation

  def costPenalty(ind: Individual): Double

  def fitnessEvaluation(ind: Individual): (IndividualEvaluation, Stream[IS[EValue]]) = {
    val inputStream = individualToPattern(ind)
    val PerformanceEvalResult(performance, info) = evaluation.evaluateAPattern(inputStream)
    val sp = sizePenaltyFactor(ind)
    val fitness = sp * performance * costPenalty(ind) + (if(isTooLarge(ind)) -0.1 else 0.0)
    IndividualEvaluation(fitness, performance, info) -> inputStream.map(_._2)
  }

  def showIndData(data: IndividualData[Individual]): String = {
    s"${data.evaluation.showAsLinearExpr} -> ${showIndividual(data.ind)}"
  }

  def showPopulation(pop: Population[Individual]): String = {
    pop.individuals.map {showIndData}.mkString("{",",","}")
  }

  def populationAverageSize(individuals: IS[Individual]): Double = {
    individuals.map{ ind =>
      representationSize(ind)
    }.sum/individuals.size
  }

  def frequencyStat(individuals: IS[Individual]): IS[(String, Int)] = {
    import collection.mutable
    val map = mutable.HashMap[String, Int]()
    for(
      ind <- individuals;
      expr <- individualExprs(ind);
      (_, e) <- Expr.subExprs(expr)
    ){
      val s = e match {
        case ENode(f,_) => f.name
        case t: ETerminal => Expr.linearShow(t)
      }
      map(s) = map.getOrElse(s, 0) + 1
    }
    map.toIndexedSeq.sortBy(_._2).reverse
  }

  def frequencyRatioStat(individuals: IS[Individual]): IS[(String, Double)] = {
    val freq = frequencyStat(individuals)
    val total = freq.map(_._2).sum
    freq.map{
      case (s, f) => (s, f.toDouble/total)
    }
  }
}

object EvolutionRepresentation{
  case class IndividualEvaluation(fitness: Double, performance: Double, extraInfo: String){
    def showAsLinearExpr: String = {
      s"(fitness: ${f"$fitness%.1f"}, performance: ${f"$performance%.1f"}, info: $extraInfo)"
    }
  }

  case class IndividualHistory(parents: IS[Int], birthOp: String, historyLength: Int)

  case class IndividualData[T](ind: T, history: IndividualHistory, evaluation: IndividualEvaluation)

  case class Population[T](individuals: IS[IndividualData[T]], fitnessMap: Map[T, IndividualEvaluation])

  case class MemoryUsage(amount: Long)
}


