package patsyn

import patsyn.GeneticOperator.ExprGen
import patsyn.Runner.RunConfig

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

    require(aggressiveness >= 0 && aggressiveness <= 1.0)

    val intRange = aggressiveInterpolate(aggressiveness, 5, 500)(rand.nextDouble()).toInt
    val constRule = PartialFunction[EType, Random => EValue] {
      case EInt => r => r.nextInt(intRange)
      case EVect(_) => _ => Vector()
      case _ => ???
    }


    val safeFunctions = IS(IntComponents, VectComponents).flatMap(_.collection)
    val unsafeFunctions = IS(BitComponents, AdvancedVectComponents).
      flatMap(_.collection).filter(_ => aggressiveSigmoid(aggressiveness)(rand.nextDouble()) > 0.5)

    val stateNum = PartialFunction[EType, Int] { _ =>
      aggressiveInterpolate(aggressiveness, 1.01, 3.01)(rand.nextDouble()).toInt
    }

    val argConstRatio = rand.nextDouble() * 0.4 + 0.2

    GPEnvGenerator(constRule, safeFunctions ++ unsafeFunctions, stateNum, argConstRatio).generate(returnTypes)
  }

  def genRunConfig(rand: Random, aggressiveness: Double, stateNum: Int): RunConfig = {
    def inter(from: Double, to: Double): Double = SimpleMath.aggressiveInterpolate(aggressiveness, from, to)(rand.nextDouble())

    val singleTolerance = inter(20, 30)
    val totalTolerance = inter(2, math.max(stateNum,3).toDouble) * singleTolerance

    RunConfig(
      populationSize = inter(100,1000).toInt,
      tournamentSize = inter(2,8).toInt,
      totalSizeTolerance = totalTolerance.toInt,
      singleSizeTolerance = singleTolerance.toInt
    )
  }

  def main(args: Array[String]): Unit = {
    import StandardSystem._

    val rand = new Random(0)

    def sigmoid(x: Double) = 1.0/(1+math.exp(-x))

    for(ag <- Seq.fill(10)(sigmoid(rand.nextGaussian()))) {
      val env = genStandardEnv(rand, aggressiveness = ag)(IS(EInt, EVect(EInt)))
      val runConfig = genRunConfig(rand, ag, env.stateTypes.length)
      println(ag)
      println(env)
      println(runConfig)
    }
  }
}
