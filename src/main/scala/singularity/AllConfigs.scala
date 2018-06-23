package singularity

import singularity.Runner.RunnerConfig

import scala.util.Random

/** Provides a full description of a fuzzing problem.
  *
  * @param outputTypes the argument types taken by the target program
  * @param resourceUsage given argument values, returns the resource usage of the target program
  * @param sizeF measures the total size of the argument values
  * */
case class ProblemConfig(problemName: String,
                         outputTypes: IS[EType],
                         resourceUsage: IS[EValue] => Double,
                         sizeF: IS[EValue] => Int = _.map(_.memoryUsage.toInt).sum,
                         displayValue: IS[EValue] => String = FuzzingTaskProvider.defaultDisplayValue,
                         saveValueWithName: (IS[EValue], String) => Unit = FuzzingTaskProvider.defaultSaveValueWithName
                        )


/**
  * Contains Genetic Programming parameters
  *
  * @param totalSizeTolerance the smaller this value is, the more penalty is applied to large individuals
  *
  * @param singleSizeTolerance the smaller this value is, the more penalty is applied to individuals with
  *                            large ASTs. The difference is that [[totalSizeTolerance]] penalizes the total AST
  *                            size of a multi-state individual, while [[singleSizeTolerance]] penalizes each
  *                            internal state.
  *
  * @param exprCostPenaltyBase The smaller, the more penalty is applied to individuals with large constant expressions.
  *
  * @param crossoverP the probability of using the crossover operator. see [[MultiStateGOpLibrary.simpleCrossOp]]
  *
  * @param mutateP the probability of using the mutation operator. see [[MultiStateGOpLibrary.simpleMutateOp]]
  *
  * @param copyP the probability of using the copy operator. see [[MultiStateGOpLibrary.copyOp]]
  *
  * @param constFoldP the probability of using the constant folding operator. see [[MultiStateGOpLibrary.constantFolding]]
  */
case class GPConfig(populationSize: Int = 500,
                    tournamentSize: Int = 7,
                    totalSizeTolerance: Int = 60,
                    singleSizeTolerance: Int = 30,
                    exprCostPenaltyBase: Double = 0.95,
                    crossoverP: Double = 0.4,
                    mutateP: Double = 0.5,
                    copyP: Double = 0.05,
                    constFoldP: Double = 0.05
                   ){
  def show: String = {
    s"""
       |populationSize: $populationSize
       |tournamentSize: $tournamentSize
       |totalSizeTolerance: $totalSizeTolerance
       |singleSizeTolerance: $singleSizeTolerance
       |exprCostPenaltyBase: $exprCostPenaltyBase,
       |crossoverP: $crossoverP
       |mutateP: $mutateP
       |copyP: $copyP
       |constFoldP: $constFoldP
     """.stripMargin
  }
}

sealed trait EvalSizePolicy{
  /** Returns the size used for fitness evaluation */
  def genSize(random: Random): Int

  /** Returns a fixed reference size used for measuring purpose */
  def referenceSize: Int
}

case class FixedEvalSize(sizeOfInterest: Int) extends EvalSizePolicy{
  /** Returns the size used for fitness evaluation, equals to [[referenceSize]] */
  def genSize(random: Random) = sizeOfInterest

  /** Returns a fixed reference size used for measuring purpose */
  def referenceSize: Int = sizeOfInterest
}

case class VariedEvalSize(referenceSize: Int, f: Random => Int, display: String) extends EvalSizePolicy{
  def genSize(random: Random) = f(random)

  override def toString = s"VariedEvalSize: $display"
}

object VariedEvalSize{
  def choppedGaussian(rand: Random, baseSize: Int,
                      gaussianChop: (Double, Double) = (-2,2),
                      expBase: Double = 2, powerE: Double = 0.5): VariedEvalSize = {
    VariedEvalSize(
      baseSize,
      rand => {
      (SimpleMath.expChoppedGaussian(gaussianChop, expBase, powerE)(rand) * baseSize).toInt
    },
      s"baseSize = $baseSize, chopMargin = $gaussianChop, base = $expBase")
  }
}

/** Corresponds to Cost models described in the Singularity paper. Only [[ResourceUsagePolicy.SimpleEvaluationPolicy]]
  * performs well on all benchmarks. */
sealed trait ResourceUsagePolicy

object ResourceUsagePolicy{

  /** Uses the performance at a fixed fuzzing size as evaluation. When [[windowSize]] is set to `n` > 1, takes the
    * maximal resource usage of the last `n` elements in the input pattern as evaluation. */
  case class SimpleEvaluationPolicy(windowSize: Int = 1) extends ResourceUsagePolicy

  /** Fits a power law and evaluate usage at size = scaleFactor * sizeOfInterest */
  case class HybridEvaluationPolicy(minPointsToUse: Int = 8,
                                    maxPointsToUse: Int = 14,
                                    scaleFactor: Double = 10.0,
                                    maxIter: Int = 100) extends ResourceUsagePolicy

  /** Fits a power law and returns the power as evaluation result */
  case class PowerLawEvaluationPolicy(minPointsToUse: Int = 8,
                                    maxPointsToUse: Int = 14,
                                    maxIter: Int = 100) extends ResourceUsagePolicy
}

/**
  * Parameters related to the execution of Genetic Programming.
  *
  * @param evalSizePolicy    the input size used for fitness evaluation. Use [[singularity.FixedEvalSize]] as a default.
  * @param threadNum         the number of threads used during fitness evaluation of a whole population
  * @param timeLimitInMillis if any input causes the target program to run more than this amount of time, saves the
  *                          corresponding input/individual and stops fuzzing
  * @param maxNonIncreaseGen stops fuzzing when there has not been any fitness improvement (on the best individual
  *                          of each generation) for more than this amount of generations
  * @param maxFuzzingTimeSec stops fuzzing when after this amount of total fuzzing time
  * @param resourcePolicy    Corresponds to Cost models described in the Singularity paper. Only
  *                          [[ResourceUsagePolicy.SimpleEvaluationPolicy]] performs well on all benchmarks.
  */
case class ExecutionConfig(evalSizePolicy: EvalSizePolicy = FixedEvalSize(300),
                           threadNum: Int = 1,
                           timeLimitInMillis: Int = 120000,
                           maxNonIncreaseGen: Option[Int] = Some(150),
                           maxFuzzingTimeSec: Option[Long] = None,
                           resourcePolicy: ResourceUsagePolicy = ResourceUsagePolicy.SimpleEvaluationPolicy()
                          ){
  def show: String = {
    s"""
       |sizeOfInterest: $evalSizePolicy
       |threadNum：$threadNum
       |timeLimitInMillis：$timeLimitInMillis
       |maxNonIncreaseGen：$maxNonIncreaseGen
       |maxFuzzingTimeSec: $maxFuzzingTimeSec
       |resourcePolicy: $resourcePolicy
     """.stripMargin
  }
}

case class AllConfigs(runnerConfig: RunnerConfig,
                      gpConfig: GPConfig,
                      execConfig: ExecutionConfig) {

def withIoIdAndSeed(ioId: Int, seed: Int): AllConfigs = {
    this.copy(
      runnerConfig = runnerConfig.copy(ioId = ioId, randomSeed = seed)
    )
  }

  def show: String = {
    s"""
       |[Runner config]
       |${runnerConfig.show}
       |[GP config]
       |${gpConfig.show}
       |[execution config]
       |${execConfig.show}
       """.stripMargin
  }
}

object AllConfigs{
  def default = AllConfigs(RunnerConfig(), GPConfig(), ExecutionConfig())
}
