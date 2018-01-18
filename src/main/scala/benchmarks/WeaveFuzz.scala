package benchmarks

import patsyn.ProblemConfig
import patsyn.Runner.RunnerConfig
import patsyn._
import patsyn.StandardSystem._
import weave_original.Weave

import scala.util.Random


object WeaveFuzz {
  def weaveConfig = {
    def getArgumentsForWeave(args: IS[EValue]) = args match{
      case IS(VectValue(vec1), VectValue(vec2), IntValue(n)) =>
        val posN = SimpleMath.wrapInRange(n, 27-3)+3
        def intToChar(n: Int): Char = {
          val x = SimpleMath.wrapInRange(n, posN * 2)
          if(x<posN) ('z' - x).toChar
          else ('Z' - (x-posN)).toChar
        }

        val s1 = String.valueOf(vec1.map{ i => intToChar(i.asInstanceOf[IntValue].value)}.toArray)
        val s2 = String.valueOf(vec2.map{ i => intToChar(i.asInstanceOf[IntValue].value)}.toArray)
        (s1, s2, posN+1)
    }

    ProblemConfig(
      problemName = "weave time",
      outputTypes = IS(EVect(EInt), EVect(EInt), EInt),
      resourceUsage = { args =>
          val (s1, s2, n) = getArgumentsForWeave(args)
          CostMeasurement.measureCost{
            val w1 = new Weave(s1, n)
            val w2 = new Weave(s2, n)
            w1.isEquivalent(w2)
          }
      },
      displayValue = args => getArgumentsForWeave(args).toString,
      sizeF = {
        case IS(VectValue(vec1), VectValue(vec2), _) =>
          math.max(vec1.length, vec2.length)
      },
      saveValueWithName = { (args, name) =>
        FuzzingTaskProvider.defaultSaveValueWithName(args, name)
        FileInteraction.writeToFile(name+"_print.txt")(getArgumentsForWeave(args).toString)
      }
    )
  }

  def runWeave(seed: Int, useGUI: Boolean): Unit = {
    val rand = new Random(seed)
    Supernova.standard.fuzzProblem(
      weaveConfig,
      RunnerConfig().copy(randomSeed = seed, ioId = seed, useGUI = useGUI),
      ExecutionConfig(evalSizePolicy = FixedEvalSize(50), timeLimitInMillis = 100000), rand)
  }

  def main(args: Array[String]): Unit = {
    implicit val classPath: FileInteraction.ClassPath = FileInteraction.getClassPath(inIDE = true)

//    SimpleMath.processMap(args, 2 to 40, processNum = 6, this){
//      i => runWeave(seed = i, useGUI = true)
//    }
//    runWeave(seed = 3, useGUI = true)

    testManually()
  }

  def testManually(): Unit ={
    val w1 = new Weave("thYPGxofWNEvmdULCtkbSJAriZQHypgXOFwneVMDulcTKBsja", 27)
    val w2 = new Weave("qykYPGxofWNEvmdULCtkbSJAriZQHypgXOFwneVMDulcTKBsja", 27)
    w1.isEquivalent(w2)
  }
}
