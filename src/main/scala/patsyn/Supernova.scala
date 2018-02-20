package patsyn

import benchmarks.{AlgorithmExamples, SlowfuzzExamples}
import patsyn.GeneticOperator.ExprGen
import patsyn.Runner.RunnerConfig
import patsyn.StandardSystem.{EInt, EVect, IntValue, VectValue}

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


class Supernova(extendedConstRule: (PartialFunction[EType, Random => EValue] => PartialFunction[EType, Random => EValue]),
                extraSafeFunctions: IS[EFunction],
                extraUnsafeFunctions: IS[EFunction]
               ){

  def genStandardEnv(rand: Random, aggressiveness: Double)(returnTypes: IS[EType]): GPEnvironment = {
    import StandardSystem._
    import SimpleMath.{aggressiveInterpolate, aggressiveSigmoid}

    val intRange = aggressiveInterpolate(aggressiveness, 5, 500)(rand.nextDouble()).toInt
    lazy val exRule = extendedConstRule(constRule)
    lazy val constRule: PartialFunction[EType, Random => EValue] = PartialFunction[EType, Random => EValue] {
      case any if exRule.isDefinedAt(any) => exRule(any)
      case EInt => r => r.nextInt(intRange)
      case EVect(_) => _ => Vector()
      case EGraph(_) => _ => GraphValue.empty
      case EPair(a,b) => r => (constRule(a)(r), constRule(b)(r))
      case EByteArray(i) => r => ByteArrayValue(IS.fill(i){ r.nextInt(256).toByte})
      case EUnit => _ => UnitValue
      case other => throw new Exception(s"Unsupported EType $other encountered in Supernova.")
    }


    val safeFunctions =
      IS(IntComponents, VectComponents, GraphComponents, PairComponents).flatMap(_.collection) ++ extraSafeFunctions
    val unsafeFunctions = (IS(BitComponents, AdvancedVectComponents).
      flatMap(_.collection) ++ extraUnsafeFunctions).filter(_ => aggressiveSigmoid(aggressiveness)(rand.nextDouble()) > 0.5)

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
      populationSize = inter(250,1000).toInt,
      tournamentSize = rand.nextInt(9-3)+3,
      totalSizeTolerance = totalTolerance.toInt,
      singleSizeTolerance = singleTolerance.toInt,
      mutateP = inter(0.4,0.7),
      crossoverP = 0.5,
      copyP = 0.05+0.1-inter(0.0,0.1),
      constFoldP = 0.04 + inter(0.0, 0.06)
    )
  }

  def genGPParameters(outputTypes: IS[EType], rand: Random, aggressiveness: Option[Double] = None): (GPEnvironment, GPConfig) = {
    import SimpleMath._

    val ag = aggressiveness.getOrElse {
      sigmoid {
        rand.nextDouble()
        rand.nextDouble()
        rand.nextGaussian()
      }
    }

    val env = genStandardEnv(rand, aggressiveness = ag)(outputTypes)
    val gpConfig = genGPConfig(rand, ag, env.stateTypes.length)

    (env, gpConfig)
  }

  def fuzzTask(name: String, taskProvider: FuzzingTaskProvider, runnerConfig: RunnerConfig, execConfig: ExecutionConfig, rand: Random): Unit = {
    taskProvider.runAsProbConfig(name) { problemConfig =>
      fuzzProblem(problemConfig, runnerConfig, execConfig, rand)
    }
  }

  def fuzzProblem(problemConfig: ProblemConfig, runnerConfig: RunnerConfig, execConfig: ExecutionConfig, rand: Random, aggressiveness: Option[Double] = None): Unit ={
    val (env, gpConfig) = genGPParameters(problemConfig.outputTypes, rand, aggressiveness)
    Runner.run(problemConfig, env, RunConfig(runnerConfig, gpConfig, execConfig))
  }
}

object Supernova{

  val standardSupernova = new Supernova(extendedConstRule = _ => PartialFunction.empty, IS(), IS())


  def main(args: Array[String]): Unit = {
    val seed = 2
//    val workingDir = FileInteraction.getWorkingDir(seed)
    val rand = new Random(seed)
    val size = 100
    val sizePolicy = FixedEvalSize(size)

    val prob = SlowfuzzExamples.phpHashExample(size)(FileInteraction.getWorkingDir(1))
//    FuzzingTaskProvider.quickSortMiddlePivotExample.runAsProbConfig("quickSortMiddle"){ prob =>
        standardSupernova.fuzzProblem(prob,
          RunnerConfig().copy(randomSeed = seed, ioId = seed, useGUI = true),
          ExecutionConfig().copy(evalSizePolicy = sizePolicy, resourcePolicy = ResourceUsagePolicy.FittingEvaluationPolicy()),
          rand = rand)
//    }
  }
}
