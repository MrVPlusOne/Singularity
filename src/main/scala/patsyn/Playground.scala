package patsyn

import benchmarks.GuavaExamples
import patsyn.Runner.RunnerConfig

import scala.util.Random

object GraphSample {
  import StandardSystem._
  import IntComponents._

  val growEdge = GraphComponents.growEdge.concretize(IS(EInt))
  val addNode = GraphComponents.addNode.concretize(IS(EInt))
  val emptyGraph = GraphComponents.emptyGraph.concretize(IS(EInt))
  val bridgeEdge = GraphComponents.bridgeEdge.concretize(IS(EInt))

  implicit def intConst(i: Int): EConst = EConst(EInt, i)

  val seeds: IS[Expr] = IS(
    2,
    bridgeEdge(addNode(addNode(emptyGraph())), 1, 0, 1)
  )

  val arg0 = EArg(0, EInt)
  val arg1 = EArg(1, EGraph(EInt))

  val iters: IS[Expr] = IS(
    inc(arg0),
    bridgeEdge(growEdge(arg1, dec(arg0), Int.MaxValue), arg0, 0, 1)
  )

  val out: IS[Expr] = IS(1, 0, arg1)

  val ind = MultiStateInd(seeds++iters++out, nStates = 2)
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
    val indicies = PatternPlot.randomSelectFrom((0 until sizeOfInterest), maxPoints = 30, random = random)

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
      Supernova.fuzzProblem(
        config,
        RunnerConfig().copy(randomSeed = ioId, ioId = ioId, useGUI = true),
        ExecutionConfig(evalSizePolicy = VariedEvalSize.choppedGaussian(rand, 400)), rand)
    }
  }

  def main(args: Array[String]): Unit = {
//    SimpleMath.processMap(args,
//      0 to 40, processNum = 14,
//      mainClass = this) {
//      ioId =>
//        run(ioId)
//    }

    run(6)
  }
}
