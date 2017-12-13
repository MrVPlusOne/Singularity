package benchmarks

import patsyn.StandardSystem._
import patsyn._

object SlowfuzzExamples {

  def nativeExample(execName: String)(inputSize: Int)(workingDir: String) = {
    val native = new FuzzingTaskProvider.NativeExample(execName, workingDir)
    ProblemConfig(
      problemName =
        if (execName.endsWith("_int"))
          s"${execName}_$inputSize"
        else
          s"${execName}_int_$inputSize",
      outputTypes = IS(EVect(EInt)),
      sizeF = {
        case IS(VectValue(v)) => v.length
      },
      resourceUsage = {
        case IS(VectValue(v)) =>
          val data = FuzzingTaskProvider.toIntVect(v).map(x => x.toByte).toArray
          native.writeByteArrayRunNativeGetCost(data)
      },
      saveValueWithName = (value: IS[EValue], name: String) => {
        value match {
          case IS(VectValue(v)) =>
            val data = FuzzingTaskProvider.toIntVect(v).map(x => x.toByte).toArray
            val fileName = s"$name.bin"
            FileInteraction.writeToBinaryFile(fileName)(data)
        }
      }
    )
  }

  def insertionSortIntExample = nativeExample("isort_int") _

  def appleQsortIntExample = nativeExample("appleqsort_int") _

  def bsdQsortIntExample = nativeExample("bsdqsort_int") _

  def gnuQsortIntExample = nativeExample("gnuqsort_int") _

  def pgQsortIntExample = nativeExample("pgqsort_int") _

  def slowfuzzQsortIntExample = nativeExample("qsort_int") _

  def phpHashExample = nativeExample("phphash") _

  def pcreExample(regexId: Int)(inputSize: Int)(workingDir: String) = {
    val native = new FuzzingTaskProvider.NativeExample("pcre_str", workingDir)
    ProblemConfig(
      problemName = "pcre_str",
      outputTypes = IS(EVect(EInt)),
      sizeF = {
        case IS(VectValue(v)) => v.length
      },
      resourceUsage = {
        case IS(VectValue(v)) =>
          val data = FuzzingTaskProvider.toIntVect(v).map(x => x.toByte).toArray
          native.withWriteByteArray(data, "input", (inputFileName: String) => {
            native.runNativeGetCost(s"$inputFileName $regexId")
          })
      },
      saveValueWithName = (value: IS[EValue], name: String) => {
        value match {
          case IS(VectValue(v)) =>
            val data = FuzzingTaskProvider.toIntVect(v).map(x => x.toByte).toArray
            val fileName = s"$name.bin"
            FileInteraction.writeToBinaryFile(fileName)(data)
        }
      }
    )
  }
}
