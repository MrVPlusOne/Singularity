package patsyn


import patsyn.EvolutionaryOptimizer.IndividualEvaluation

import scala.util.Random

object EvolutionaryOptimizer{
  case class IndividualEvaluation(fitness: Double, performance: Double){
    def showAsLinearExpr: String = {
      s"(fitness: ${f"$fitness%.1f"}, performance: ${f"$performance%.1f"})"
    }
  }


}

trait EvolutionaryOptimizer[Individual] {
  type GOp = GeneticOperator[Individual]

  def showIndividual(ind: Individual): String
  def representationSize(ind: Individual): Double
  def individualExprs(ind: Individual): Seq[Expr]

  case class IndividualHistory(parents: IS[Int], birthOp: GOp, historyLength: Int)

  case class IndividualData(ind: Individual, history: IndividualHistory, evaluation: IndividualEvaluation){
    def showAsLinearExpr: String = {
      s"${evaluation.showAsLinearExpr} -> ${showIndividual(ind)}"
    }
  }

  case class Population(individuals: IS[IndividualData], fitnessMap: Map[Individual, IndividualEvaluation]){
    def showLinearExprs: String = individuals.map {_.showAsLinearExpr}.mkString("{",",","}")

    def averageSize: Double = individuals.map{ eval =>
      representationSize(eval.ind)
    }.sum/individuals.size

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

    def frequencyStat: IS[(String, Int)] = {
      import collection.mutable
      val map = mutable.HashMap[String, Int]()
      for(
        eval <- individuals;
        ind = eval.ind;
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

    def frequencyRatioStat: IS[(String, Double)] = {
      val freq = frequencyStat
      val total = freq.map(_._2).sum
      freq.map{
        case (s, f) => (s, f.toDouble/total)
      }
    }

    def bestSoFar: IndividualData = {
      individuals.maxBy(_.evaluation.fitness)
    }
  }

  def optimize(populationSize: Int, tournamentSize: Int, neighbourSize: Int,
               initOperator: GOp,
               operators: IS[(GOp, Double)],
               evaluation: Individual => IndividualEvaluation,
               threadNum: Int,
               randSeed: Int
                     ): Iterator[Population] = {
    require(neighbourSize*2+1 <= populationSize, "Neighbour size too large.")

    import scala.collection.parallel
    import parallel._
    def toPar[A](seq: Seq[A]): ParSeq[A] = {
      val p = seq.par
      p.tasksupport = new ForkJoinTaskSupport(new scala.concurrent.forkjoin.ForkJoinPool(threadNum))
      p
    }

    val operatorPCF: IS[(GOp, Double)] = {
      val normalizeFactor = operators.map(_._2).sum
      var acc = 0.0
      operators.map{
        case (op, p) =>
          acc += p
          (op, acc / normalizeFactor)
      }
    }

    val random = new Random(randSeed) // used through multiple generations

    val initPop = {
      val individuals = (0 until populationSize).map(_ => initOperator.operate(random, IS()))
      val fitnessMap = toPar(individuals.distinct).map(ind => ind -> evaluation(ind)).toIndexedSeq.toMap

      Population(individuals.map{ind =>
        val eval = fitnessMap(ind)
        IndividualData(ind, IndividualHistory(IS(), initOperator, historyLength = 0), eval)
      }, fitnessMap)
    }


    Iterator.iterate(initPop){ pop =>
      def tournamentResult(position: Int): (IndividualData, Int) = {
        val candidates = IS.fill(tournamentSize){
          val offset = random.nextInt(neighbourSize*2+1)-neighbourSize
          val idx = SimpleMath.wrapInRange(offset + position, populationSize)
          pop.individuals(idx) -> idx
        }
        candidates.maxBy{ case (data, _) =>
          data.evaluation.fitness
        }
      }

      val newIndsAndHistory = for (i <- 0 until populationSize) yield {
        val geneticOp = {
          val x = random.nextDouble()

          operatorPCF.find {
            case (op, pAcc) => x < pAcc
          }.getOrElse(operatorPCF.last)._1
        }

        val participates = IS.fill(geneticOp.arity) {
          tournamentResult(i)
        }
        val newInd = geneticOp.operate(random, participates.map(_._1.ind))
        val historyLen = if(participates.isEmpty) 0 else participates.map(_._1.history.historyLength).max + 1
        val newHistory = IndividualHistory(participates.map(_._2), geneticOp, historyLen)
        newInd -> newHistory
      }

      val newInds = newIndsAndHistory.map(_._1)
      val newFitnessMap = toPar(newInds.distinct).map{ind =>
        ind -> pop.fitnessMap.getOrElse(ind, default = evaluation(ind))
      }.toIndexedSeq.toMap

      val newData = newIndsAndHistory.indices.map{i =>
        val (ind, history) = newIndsAndHistory(i)
        val eval = newFitnessMap(ind)
        IndividualData(ind, history, eval)
      }

      Population(newData, newFitnessMap)
    }
  }

}
