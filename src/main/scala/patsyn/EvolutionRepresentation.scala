package patsyn


import patsyn.EvolutionRepresentation._


trait EvolutionRepresentation[Individual] {
  def showIndividual(ind: Individual): String
  def representationSize(ind: Individual): Double
  def individualExprs(ind: Individual): Seq[Expr]

  def individualToPattern(ind: Individual): Stream[(MemoryUsage, IS[EValue])]

  def sizePenaltyFactor(ind: Individual): Double

  def evaluation: PerformanceEvaluation

  def fitnessEvaluation(ind: Individual): (IndividualEvaluation, Stream[IS[EValue]]) = {
    val inputStream = individualToPattern(ind)
    val performance = evaluation.evaluateAPattern(inputStream)
    val fitness = sizePenaltyFactor(ind) * performance
    IndividualEvaluation(fitness, performance) -> inputStream.map(_._2)
  }

  def showIndData(data: IndividualData[Individual]): String = {
    s"${data.evaluation.showAsLinearExpr} -> ${showIndividual(data.ind)}"
  }

  def showPopulation(pop: Population[Individual]): String = {
    pop.individuals.map {showIndData}.mkString("{",",","}")
  }

  def populationAverageSize(pop: Population[Individual]): Double = {
    pop.individuals.map{ eval =>
      representationSize(eval.ind)
    }.sum/pop.individuals.size
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
  case class IndividualEvaluation(fitness: Double, performance: Double){
    def showAsLinearExpr: String = {
      s"(fitness: ${f"$fitness%.1f"}, performance: ${f"$performance%.1f"})"
    }
  }

  case class IndividualHistory[T](parents: IS[Int], birthOp: GeneticOperator[T], historyLength: Int)

  case class IndividualData[T](ind: T, history: IndividualHistory[T], evaluation: IndividualEvaluation)

  case class Population[T](individuals: IS[IndividualData[T]], fitnessMap: Map[T, IndividualEvaluation]){

    lazy val averageFitness: Double = {
      individuals.map(_.evaluation.fitness).sum/individuals.size
    }

    lazy val fitnessStdDiv: Double = {
      val aveFit = averageFitness
      math.sqrt{
        individuals.map(e => SimpleMath.square(e.evaluation.fitness - aveFit)).sum / individuals.length
      }
    }

    lazy val averagePerformance: Double = {
      individuals.map(_.evaluation.performance).sum/individuals.size
    }

    def bestSoFar: IndividualData[T] = {
      individuals.maxBy(_.evaluation.fitness)
    }
  }

  case class MemoryUsage(amount: Long)
}


