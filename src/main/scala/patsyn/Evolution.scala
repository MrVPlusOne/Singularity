package patsyn

import scala.util.Random
import Evolution._

object Evolution {
  trait Provider[T]{
    def provide(r: Random): T
  }


  case class Individual(seed: IS[Expr], iter: IS[Expr]){

    def showAsLinearExpr: String = {
      s"{seed: ${seed.map(Expr.linearShow).mkString("< ", " | ", " >" )} ; " +
        s"iter: ${iter.map(Expr.linearShow).mkString("< ", " | ", " >" )}}"
    }
  }

  case class IndividualEvaluation(ind: Individual, fitness: Double, performance: Double){
    def showAsLinearExpr: String = {
      s"(fitness: ${f"$fitness%.1f"}, performance: ${f"$performance%.1f"}) -> ${ind.showAsLinearExpr}"
    }
  }

  case class Population(evaluations: IS[IndividualEvaluation]){
    def showLinearExprs: String = evaluations.map {_.showAsLinearExpr}.mkString("{",",","}")

    def averageSize: Double = evaluations.map{ eval =>
      val seed = eval.ind.seed
      val iter = eval.ind.iter
      (seed++iter).map(_.astSize).sum.toDouble
    }.sum/evaluations.size

    def averageFitness: Double = {
      evaluations.map(_.fitness).sum/evaluations.size
    }

    def averagePerformance: Double = {
      evaluations.map(_.performance).sum/evaluations.size
    }

    def frequencyStat: IS[(String, Int)] = {
      import collection.mutable
      val map = mutable.HashMap[String, Int]()
      for(
        eval <- evaluations;
        ind = eval.ind;
        expr <- ind.iter;
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

    def bestSoFar: IndividualEvaluation = {
      evaluations.maxBy(_.fitness)
    }
  }

}

class Evolution {

  def evolveAFunction(populationSize: Int, tournamentSize: Int, randSeed: Int,
                      initOperator: GeneticOperator,
                      operators: IS[(GeneticOperator, Double)],
                      evaluation: Individual => IndividualEvaluation,
                     ): Iterator[Population] = {
    val operatorPCF: IS[(GeneticOperator, Double)] = {
      val normalizeFactor = operators.map(_._2).sum
      var acc = 0.0
      operators.map{
        case (op, p) =>
          acc += p
          (op, acc / normalizeFactor)
      }
    }

    val random = new Random(randSeed)

    val initPop = Population((0 until populationSize).map{i =>
      evaluation(initOperator.operate(random, IS()))
    })

    Iterator.iterate(initPop){pop =>
      def tournamentResult(): Individual = {
        val candidates = IS.fill(tournamentSize){
          pop.evaluations(random.nextInt(populationSize))
        }
        candidates.maxBy(ind => ind.fitness).ind
      }

      val newInds = for(i <- 0 until populationSize) yield {
        val geneticOp = {
          val x = random.nextDouble()

          operatorPCF.find{
            case (op, pAcc) => x < pAcc
          }.getOrElse(operatorPCF.last)._1
        }

        val participates = IS.fill(geneticOp.arity){
          tournamentResult()
        }
        evaluation(geneticOp.operate(random, participates))
      }
      Population(newInds)
    }
  }
}
