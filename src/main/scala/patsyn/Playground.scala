package patsyn

import benchmarks.{GuavaExamples, JGraphTExamples}
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

  def previous(): Unit = {
    import visual.{MonitorPanel, PatternPlot, ListPlot}


    var s0 = 403
    var s1 = 280
    val sizeOfInterest = 1000

    val pairs = for(_ <- 0 until sizeOfInterest) yield {
      s0 = s1
      s1 = (226<<24) + s1
      (s0, s1)
    }

    val config = GuavaExamples.immutableBiMap_copyOf
    val random = new Random(1)
    val indicies = SimpleMath.randomSelectFrom((0 until sizeOfInterest), maxPoints = 30, random = random)

    import StandardSystem._
    val points = indicies.map{
      i =>
        val input = VectValue(pairs.take(i).map{ case (x1,x2) => PairValue((x1,x2))}.toVector)
        val size = config.sizeF(IS(input))
        val resource = config.resourceUsage(IS(input))
        (size.toDouble, resource)
    }

    import javax.swing.JFrame

    import benchmarks.GuavaExamples
    import patsyn.MultiStateRepresentation.individualToPattern
    import patsyn._


    val frame = new JFrame("Monitor") {
      setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)
      setVisible(true)
    }

    var data = IndexedSeq[(Double, Double)]()
    points.foreach{ xy =>
      data :+= xy
      val chart = ListPlot.plot("pattern" -> data)("test", "size", "resource")
      frame.setContentPane(new MonitorPanel(Some(chart), margin = 10, plotSize = (600,450)))
      frame.pack()
    }
    println(data)

    println("Evaluation finished.")
  }


  def run(ioId: Int): Unit ={
    val rand = new Random(ioId)
    FuzzingTaskProvider.phpHashCollisionExample.runAsProbConfig("PhpHash"){ config =>
     Supernova.standardSupernova.fuzzProblem(
        config,
        RunnerConfig().copy(randomSeed = ioId, ioId = ioId, useGUI = false),
        ExecutionConfig(evalSizePolicy = VariedEvalSize.choppedGaussian(rand, 400)), rand)
    }
  }

  def main(args: Array[String]): Unit = {

    import patsyn.SimpleMath._

    val ys = ((1 to 10) ++ (11 to 20).map(x => 2*x-10)).map(_.toDouble)
    println(ys)

    val obs = Vector(1.6042, 2.09113, 2.75512, 3.56867, 4.51526, 5.58344, 6.76462,
    8.05203, 9.44014, 10.9243, 12.5006, 14.1656, 15.9162, 17.7497,
    19.6638, 21.6562, 23.7249, 25.8682, 28.0843, 30.3717)

    val ranObs = Vector(1.53002, 2.16833, 2.66912, 3.49463, 4.63557, 5.82693, 6.80915,
    8.15374, 9.50951, 10.8812, 11.9085, 14.8277, 15.5781, 16.9295,
    18.6827, 20.7155, 22.5459, 26.4805, 26.7853, 29.4198)

    println{
     // rSquared(obs, ranObs, Vector.fill(20)(1.0/20.0), 1.0)
    }

  }
}
