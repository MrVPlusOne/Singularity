package benchmarks

import java.io.ByteArrayOutputStream

import patsyn._
import StandardSystem._
import patbench.commons.compress.compressors.{CompressorStreamFactory => CF}
import patbench.commons.compress.archivers.{ArchiveStreamFactory => AF}
import BenchmarkSet._
import patbench.commons.math3.fitting.{AbstractCurveFitter, HarmonicCurveFitter, PolynomialCurveFitter, WeightedObservedPoints}
import patsyn.Runner.RunnerConfig

import scala.util.Random


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

  def curveFittingProblem(name: String, fitter: AbstractCurveFitter): ProblemConfig = {
    ProblemConfig(
      problemName = name,
      outputTypes = IS(EVect(EPair(EInt, EInt))),
      resourceUsage = {
        case IS(VectValue(vec)) =>
          val points = new WeightedObservedPoints()
          vec.foreach {
            case PairValue((IntValue(x), IntValue(y))) =>
              points.add(x, y)
          }

          measureCost{
            fitter.fit(points.toList)
          }
      }
    )
  }

  def runExample(seed: Int, useGUI: Boolean): Unit = {
    val rand = new Random(seed)
    Supernova.standardSupernova.fuzzProblem(
      curveFittingProblem("commons.polyFit5", PolynomialCurveFitter.create(5)),
      RunnerConfig().copy(randomSeed = seed, ioId = seed, useGUI = useGUI),
      ExecutionConfig(evalSizePolicy = FixedEvalSize(500)), rand)
  }

  def main(args: Array[String]): Unit = {
    runExample(2, true)

//    SimpleMath.processMap(args,
//          0 to 60, processNum = 10,
//          mainClass = this){
//          i => runExample(i + 100, useGUI = false)
//        }
  }

}
