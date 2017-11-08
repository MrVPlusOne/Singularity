package patsyn

import patsyn.EvolutionRepresentation.{IndividualData, IndividualEvaluation, IndividualHistory, Population}

import scala.util.Random

case class EvolutionaryOptimizer[Individual](representation: EvolutionRepresentation[Individual]) {
  type GOp = GeneticOperator[Individual]

  def optimize(populationSize: Int, tournamentSize: Int, neighbourSize: Option[Int] = None,
               initOperator: GOp,
               operators: IS[(GOp, Double)],
               indEval: Individual => IndividualEvaluation,
               threadNum: Int,
               randSeed: Int,
               evalProgressCallback: Int => Unit,
               timeLimitInMillis: Int, timeoutCallback: Individual => IndividualEvaluation
                     ): Iterator[Population[Individual]] = {

    require(threadNum>=1)
    neighbourSize.foreach(ns => require(ns*2+1 <= populationSize, "Neighbour size too large."))

    var progress = 0
    def evaluation(individual: Individual): IndividualEvaluation = {
      import scala.concurrent._
      import scala.concurrent.duration._
      import scala.concurrent.ExecutionContext.Implicits.global
      try {
        Await.result(Future{
          val result = indEval(individual)
          this.synchronized {
            progress += 1
            evalProgressCallback(progress)
          }
          result
        }
        ,timeLimitInMillis.milliseconds)
      } catch {
        case _: TimeoutException =>
          timeoutCallback(individual)
      }
    }

    import scala.collection.parallel
    import parallel._
    val taskSupport = new ForkJoinTaskSupport(new scala.concurrent.forkjoin.ForkJoinPool(threadNum))
    def parExecute[A, B](seq: Seq[A])(f: A => B): IS[B] = {
      if(threadNum>1) {
        val p = seq.par
        p.tasksupport = taskSupport
        p.map(f).toIndexedSeq
      }else{
        seq.map(f).toIndexedSeq
      }
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
      val fitnessMap = parExecute(individuals.distinct)(ind => ind -> evaluation(ind)).toMap

      Population(individuals.map{ind =>
        val eval = fitnessMap(ind)
        IndividualData(ind, IndividualHistory(IS(), initOperator.name, historyLength = 0), eval)
      }, fitnessMap)
    }
    progress = 0


    Iterator.iterate(initPop){ pop =>
      def tournamentResult(position: Int): (IndividualData[Individual], Int) = {
        val candidates = IS.fill(tournamentSize) {
          val idx = neighbourSize match {
            case Some(ns) =>
              val offset = random.nextInt(ns * 2 + 1) - ns
              SimpleMath.wrapInRange(offset + position, populationSize)
            case None => random.nextInt(populationSize)
          }
          pop.individuals(idx) -> idx
        }
        candidates.maxBy { case (data, _) =>
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
        val newHistory = IndividualHistory(participates.map(_._2), geneticOp.name, historyLen)
        newInd -> newHistory
      }

      val newInds = newIndsAndHistory.map(_._1)
      val newFitnessMap = parExecute(newInds.distinct)(ind =>
        ind -> pop.fitnessMap.getOrElse(ind, default = evaluation(ind))).toMap

      val newData = newIndsAndHistory.indices.map{i =>
        val (ind, history) = newIndsAndHistory(i)
        val eval = newFitnessMap(ind)
        IndividualData(ind, history, eval)
      }

      progress = 0
      Population(newData, newFitnessMap)
    }
  }

}
