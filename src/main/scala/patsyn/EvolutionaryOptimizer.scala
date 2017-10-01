package patsyn

import patsyn.EvolutionRepresentation.{IndividualData, IndividualEvaluation, IndividualHistory, Population}

import scala.util.Random

case class EvolutionaryOptimizer[Individual](representation: EvolutionRepresentation[Individual]) {
  type GOp = GeneticOperator[Individual]

  def optimize(populationSize: Int, tournamentSize: Int, neighbourSize: Int,
               initOperator: GOp,
               operators: IS[(GOp, Double)],
               evaluation: Individual => IndividualEvaluation,
               threadNum: Int,
               randSeed: Int
                     ): Iterator[Population[Individual]] = {
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
      def tournamentResult(position: Int): (IndividualData[Individual], Int) = {
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
