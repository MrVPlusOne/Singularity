package patsyn

import patsyn.GeneticOperator.ExprGen
import patsyn.Runner.RunnerConfig

import scala.util.Random

case class GPEnvGenerator(constRule: PartialFunction[EType, Random => EValue],
                          functions: IS[EFunction],
                          stateNUm: PartialFunction[EType, Int],
                          argConstRatio: Double
                         ) {

  def generate(returnTypes: IS[EType]): GPEnvironment = {
    val typeUniverse = returnTypes.distinct.flatMap(t => t.powerset).toSet

    val constMap = typeUniverse.map{t =>
      val rule = constRule.applyOrElse(t, (_: EType) => throw new Exception(s"No constRule for type $t"))
      t -> ExprGen[EConst](t, r => EConst(t,rule(r)))
    }.toMap

    val componentsToUse = functions.filter{
      case cf: EConcreteFunc => typeUniverse.contains(cf.returnType)
      case _ => true
    }

    val stateTypes = typeUniverse.toIndexedSeq.flatMap{t => IS.fill(stateNUm(t))(t)}

    GPEnvironment(constMap, componentsToUse, stateTypes, argConstRatio = argConstRatio, warningOn = false)
  }
}

object Sledgehammer{
  def genStandardEnv(rand: Random, aggressiveness: Double)(returnTypes: IS[EType]): GPEnvironment = {
    import StandardSystem._
    import SimpleMath.{aggressiveInterpolate, aggressiveSigmoid}

    val intRange = aggressiveInterpolate(aggressiveness, 5, 500)(rand.nextDouble()).toInt
    lazy val constRule: PartialFunction[EType, Random => EValue] = PartialFunction[EType, Random => EValue] {
      case EInt => r => r.nextInt(intRange)
      case EVect(_) => _ => Vector()
      case EGraph(_) => _ => GraphValue.empty
      case EPair(a,b) => r => (constRule(a)(r), constRule(b)(r))
      case EByteArray(i) => r => ByteArrayValue(IS.fill(i){ r.nextInt(256).toByte})
    }


    val safeFunctions = IS(IntComponents, VectComponents, GraphComponents, PairComponents).flatMap(_.collection)
    val unsafeFunctions = IS(BitComponents, AdvancedVectComponents).
      flatMap(_.collection).filter(_ => aggressiveSigmoid(aggressiveness)(rand.nextDouble()) > 0.5)

    val stateNum = PartialFunction[EType, Int] { _ =>
      aggressiveInterpolate(aggressiveness, 1.01, 3.01)(rand.nextDouble()).toInt
    }

    val argConstRatio = rand.nextDouble() * 0.4 + 0.2

    GPEnvGenerator(constRule, safeFunctions ++ unsafeFunctions, stateNum, argConstRatio).generate(returnTypes)
  }

  def genGPConfig(rand: Random, aggressiveness: Double, stateNum: Int): GPConfig = {
    def inter(from: Double, to: Double): Double = SimpleMath.aggressiveInterpolate(aggressiveness, from, to)(rand.nextDouble())

    val singleTolerance = inter(20, 30)
    val totalTolerance = inter(2, math.max(stateNum,3).toDouble) * singleTolerance

    GPConfig(
      populationSize = inter(200,1000).toInt,
      tournamentSize = inter(4,8).toInt,
      totalSizeTolerance = totalTolerance.toInt,
      singleSizeTolerance = singleTolerance.toInt,
      mutateP = inter(0.4,0.7),
      crossoverP = 0.5,
      copyP = 0.07+0.9-inter(0.0,0.9)
    )
  }

  def makeTaskProvider(random: Random, aggressiveness: Double)
                      (returnTypes: IS[EType], sizeMetric: PartialFunction[IS[EValue], Int], sizeOfInterest: Int,
                       resourceConfig: ResourceConfig): FuzzingTaskProvider = {
    val gpEnv = genStandardEnv(random, aggressiveness)(returnTypes)

    new FuzzingTaskProvider {
      val sizeF = sizeMetric

      def outputTypes = returnTypes

      override def setupTask(task: RunningFuzzingTask): Unit = resourceConfig.setup()

      override def teardownTask(task: RunningFuzzingTask): Unit = resourceConfig.teardown()

      protected def task: RunningFuzzingTask = {
        RunningFuzzingTask(sizeOfInterest, resourceConfig.resourceUsage, gpEnv)
      }
    }
  }

  def sledgehammer(outputTypes: IS[EType], rand: Random): (GPEnvironment, GPConfig) = {
    import SimpleMath._

    val ag = sigmoid{
      rand.nextDouble()
      rand.nextDouble()
      rand.nextGaussian()
    }

    val env = genStandardEnv(rand, aggressiveness = ag)(outputTypes)
    val gpConfig = genGPConfig(rand, ag, env.stateTypes.length)

    (env, gpConfig)
  }

  def sledgehammerTask(taskProvider: FuzzingTaskProvider, runnerConfig: RunnerConfig, execConfig: ExecutionConfig, rand: Random): Unit = {
    taskProvider.runAsProbConfig{ problemConfig =>
      sledgehammerProblem(problemConfig, execConfig, runnerConfig, rand)
    }
  }

  def sledgehammerProblem(problemConfig: ProblemConfig, execConfig: ExecutionConfig, runnerConfig: RunnerConfig, rand: Random): Unit ={
    val (env, gpConfig) = sledgehammer(problemConfig.outputTypes, rand)
    Runner.run(problemConfig, env, RunConfig(runnerConfig, gpConfig, execConfig))
  }

  def main(args: Array[String]): Unit = {
    val example = FuzzingTaskProvider.hashCollisionExample(HashFunc.php, 16)
    val seed = 3
    val rand = new Random(seed)
    sledgehammerTask(example, RunnerConfig().copy(randomSeed = seed, ioId = seed), ExecutionConfig(), rand)
  }
}
