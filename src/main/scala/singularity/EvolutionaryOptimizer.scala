package singularity

import singularity.EvolutionRepresentation.{IndividualData, IndividualEvaluation, IndividualHistory, Population}

import scala.util.Random

/** A generic Genetic Programming Implementation.
  * @tparam Individual The type of the individuals being optimized. Currently only [[singularity.MultiStateInd]]
  *                    are widely used.
  * @see [[singularity.MultiStateInd]], [[singularity.EvolutionRepresentation]] */
case class EvolutionaryOptimizer[Individual]() {
  type GOp = GeneticOperator[Individual]

  /** Returns a lazy stream of Populations
    * @param neighbourSize If set to None, uses the standard tournament method to select individuals; otherwise,
    *                      only individuals that are close to each other can be combined using GP operators.
    *                      Recommended to set this to None
    *
    * @param threadNum The number of threads used during indiviudal evaluation
    * @param cacheEvaluations If set to true, reuse fitness evaluation of the same individual whenever possible.
    *
    * This class is used in [[singularity.Runner]]`.run`
    * */
  def optimize(populationSize: Int,
               tournamentSize: Int,
               neighbourSize: Option[Int] = None,
               initOperator: GOp,
               operators: IS[(GOp, Double)],
               indEval: Individual => IndividualEvaluation,
               threadNum: Int,
               random: Random,
               evalProgressCallback: Int => Unit,
               cacheEvaluations: Boolean): Iterator[Population[Individual]] = {

    require(threadNum>=1)
    neighbourSize.foreach(ns => require(ns*2+1 <= populationSize, "Neighbour size too large."))

    var progress = 0
    def evaluation(individual: Individual): IndividualEvaluation = {
      val result = indEval(individual)
      this.synchronized {
        progress += 1
        evalProgressCallback(progress)
      }
      result
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

    val operatorPCF = SimpleMath.PCF(operators)


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

      val newIndsAndHistory = {
        for (i <- 0 until populationSize) yield {
          val geneticOp = operatorPCF.sample(random)

          val participates = IS.fill(geneticOp.arity) {
            tournamentResult(i)
          }
          val newInd = geneticOp.operate(random, participates.map(_._1.ind))
          val historyLen = if (participates.isEmpty) 0 else participates.map(_._1.history.historyLength).max + 1
          val newHistory = IndividualHistory(participates.map(_._2), geneticOp.name, historyLen)
          newInd -> newHistory
        }
      }

      val newInds = newIndsAndHistory.map(_._1)
      val newFitnessMap = {
        parExecute(newInds.distinct) { ind =>
          val eval = if (cacheEvaluations) {
            pop.fitnessMap.getOrElse(ind, default = evaluation(ind))
          } else evaluation(ind)
          ind -> eval
        }.toMap
      }

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
