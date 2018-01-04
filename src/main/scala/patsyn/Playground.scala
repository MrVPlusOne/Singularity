package patsyn

import patsyn.Runner.RunnerConfig

import scala.util.Random

/**
  *
[O0: EGraph(EInt)] -> #1
[O1: EInt] -> 0
[O2: EInt] -> 12
  *
[S0: EInt]{ seed: 5 ; iter: 12; seedExpr: 5 }
[S1: EGraph(EInt)]{ seed: GraphValue(0,Vector()) ; iter: addEdge(growEdge(growEdge(#1, neg(3), 3), 3, 10), 3); seedExpr: GraphValue(0,Vector()) }
  */

object GraphSample {
  import StandardSystem._
  import IntComponents._

  val growEdge = GraphComponents.growEdge.concretize(IS(EInt))
  val addNode = GraphComponents.addNode.concretize(IS(EInt))
  val emptyGraph = GraphComponents.emptyGraph.concretize(IS(EInt))
  val bridgeEdge = GraphComponents.bridgeEdge.concretize(IS(EInt))

  implicit def intConst(i: Int): EConst = EConst(EInt, i)

  val seeds: IS[Expr] = IS(
    addNode(emptyGraph())
  )

  val arg0 = EArg(0, EGraph(EInt))

  val iters: IS[Expr] = IS(
    growEdge(growEdge(arg0, neg(3),3), 3, 10)
  )

  val out: IS[Expr] = IS(arg0, 0, 12)

  val ind = MultiStateInd(seeds++iters++out, nStates = seeds.length)
}

object HashSample {
  import StandardSystem._
  import IntComponents._

  implicit def intConst(i: Int): EConst = EConst(EInt, i)
  implicit def vectConst(v: IS[Int]): EConst = EConst(EVect(EInt), VectValue(v.map(i => IntValue(i)).toVector))

  val append = VectComponents.append.concretize(IS(EInt))
  val appendVec = VectComponents.append.concretize(IS(EVect(EInt)))
  val access = VectComponents.access.concretize(IS(EInt))

  val seeds: IS[Expr] = IS(
    vectConst(IS(0, 310000)),
    EConst(EVect(EVect(EInt)), VectValue(Vector()))
  )

  val arg0 = EArg(0, EVect(EInt))
  val arg1 = EArg(1, EVect(EVect(EInt)))
  val iters: IS[Expr] = IS(
    append(append(vectConst(IS()), inc(access(arg0, 0, 0))), minus(access(arg0, 1, 0), 31)),
    appendVec(arg1, arg0)
  )

  val out: IS[Expr] = IS(arg1)
  val ind = MultiStateInd(seeds++iters++out, nStates = 2)
}

object Playground {

  def run(ioId: Int): Unit ={
    val rand = new Random(ioId)
    FuzzingTaskProvider.phpHashCollisionExample.runAsProbConfig("PhpHash"){ config =>
      Supernova.standard.fuzzProblem(
        config,
        RunnerConfig().copy(randomSeed = ioId, ioId = ioId, useGUI = false),
        ExecutionConfig(evalSizePolicy = VariedEvalSize.choppedGaussian(rand, 400)), rand)
    }
  }

  def compressNumToString(num: Int): String = {
    SimpleMath.natToList(num,26*2).map{i =>
      val c = if(i<26) 'a' + i else 'A' + i - 26
      c.toChar
    }.mkString("")
  }

  def main(args: Array[String]): Unit = {
    println("create table t20 " + (1 to 120).map(i => s"${compressNumToString(i)} int").mkString("(",",",")"))
  }
}
