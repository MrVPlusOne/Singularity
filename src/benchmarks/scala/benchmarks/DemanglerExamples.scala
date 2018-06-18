package benchmarks

import scala.util.Random

import patsyn.Runner.RunnerConfig
import patsyn.StandardSystem.{EInt, EVect, VectValue}
import patsyn._

object DemanglerExamples {
  def demanglerExample(execName: String)(workingDir: String) = {
    val native = new FuzzingTaskProvider.NativeExample(execName, workingDir)
    ProblemConfig(
      problemName = execName,
      outputTypes = IS(EVect(EInt)),
      sizeF = {
        case IS(VectValue(v)) => v.length
      },
      resourceUsage = {
        case IS(VectValue(v)) =>
          val chArray = FuzzingTaskProvider.vectIntToCharArray(v, 8).toArray
          native.writeByteArrayRunNativeGetCost(chArray.map(ch => ch.toByte))
      },
      saveValueWithName = (value: IS[EValue], name: String) => {
        value match {
          case IS(VectValue(v)) =>
            val str = FuzzingTaskProvider.vectIntToString(v, 8)
            val textFileName = s"$name.txt"
            FileInteraction.writeToFile(textFileName)(str)
        }
      }
    )
  }

  def llvmDemanglerExample = demanglerExample("llvm_demangler") _

  def lldbDemanglerExample = demanglerExample("lldb_fast_demangler") _

  def runExample(example: String => ProblemConfig, seed: Int, useGUI: Boolean): Unit = {
    val rand = new Random(seed)
    val workingDir = FileInteraction.getWorkingDir(seed)
    Supernova.standardSupernova.fuzzProblem(
      example(workingDir),
      RunnerConfig().copy(randomSeed = seed, ioId = seed, useGUI = useGUI),
      ExecutionConfig(evalSizePolicy = FixedEvalSize(500)), rand)
  }

  def main(args: Array[String]): Unit = {
    runExample(lldbDemanglerExample, 2, true)

//    SimpleMath.processMap(args,
//          0 to 60, processNum = 10,
//          mainClass = this){
//          i => runExample(lldbDemanglerExample, i, useGUI = false)
//        }
  }
}
