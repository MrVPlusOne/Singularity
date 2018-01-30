package benchmarks

import java.io.ByteArrayOutputStream

import patsyn._
import StandardSystem._
import org.apache.commons.compress.compressors.{CompressorStreamFactory => CF}
import org.apache.commons.compress.archivers.{ArchiveStreamFactory => AF}
import BenchmarkSet._


object CommonsExamples {
  def compressionProblem: ProblemConfig = {
    val algNames =
      IS[String](CF.GZIP, CF.BZIP2, CF.XZ, CF.LZMA, CF.DEFLATE)

    def getAlgName(algIdx: Int) = {
      val i = SimpleMath.wrapInRange(algIdx, algNames.length)
      algNames(i)
    }

    ProblemConfig(
      problemName = "commons.compression",
      outputTypes = IS(EVect(EInt), EInt),
      resourceUsage = {
        case IS(VectValue(vec), IntValue(algIdx)) =>
          val outStream = new ByteArrayOutputStream()
          val cStream = new CF().createCompressorOutputStream(getAlgName(algIdx), outStream)
          val bytes = vec.map{ case IntValue(i) => i.toByte}.toArray
          measureCost{
            cStream.write(bytes)
            cStream.flush()
            cStream.close()
          }
      },
      displayValue = {
        case IS(VectValue(vec), IntValue(algIdx)) =>
          getAlgName(algIdx) + ": " + vec.toString
      }
    )
  }

  @deprecated  //not working; need special data format
  def archiveProblem: ProblemConfig = {
    val algNames =
      IS[String](AF.SEVEN_Z, AF.AR, AF.CPIO, AF.TAR, AF.ZIP)

    def getAlgName(algIdx: Int) = {
      val i = SimpleMath.wrapInRange(algIdx, algNames.length)
      algNames(i)
    }

    ProblemConfig(
      problemName = "commons.archive",
      outputTypes = IS(EVect(EInt), EInt),
      resourceUsage = {
        case IS(VectValue(vec), IntValue(algIdx)) =>
          val outStream = new ByteArrayOutputStream()
          val algName = getAlgName(algIdx)
          val cStream = new AF().createArchiveOutputStream(algName, outStream)
          val bytes = vec.map{ case IntValue(i) => i.toByte}.toArray
          measureCost{
            cStream.write(bytes)
            cStream.flush()
            cStream.close()
          }
      }
    )
  }

}
