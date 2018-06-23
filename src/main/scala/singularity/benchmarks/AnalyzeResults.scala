package singularity.benchmarks

import java.util.Date

import ammonite.ops._
import singularity.measure.TimeTools
import singularity.FittingPerformanceEvaluation.PowerLawFitter
import singularity.StandardSystem.GraphValue
import singularity._

import scala.util.matching.Regex

object AnalyzeResults {
  sealed trait PerformanceState
  case object PerformanceException extends PerformanceState
  case object PerformanceTimeout extends PerformanceState
  case class PerformanceNormal(value: Double) extends PerformanceState

  def performanceToDouble(perf: PerformanceState): Double = perf match {
    case PerformanceNormal(d) => d
    case _ => Double.MaxValue
  }

  case class ResultInfo(name: String, performance: PerformanceState, ioId: Int, Seed: Int, startTime: Date, dirName: String)

  val resultInfo: Regex = """([^\[]+)\[([^\[]+)\]\[([^\[]+)\]\(([^()]+)\)""".r
  val extraInfo: Regex = """ioId=(\d+),seed=(\d+)""".r
  def parseResultInfo(dirName: String) = {

    dirName match {
      case resultInfo(name, perfS, extra, dateS) =>
        val perf = {
          perfS.split("=").last match{
            case "exception" => PerformanceException
            case "timeout" => PerformanceTimeout
            case i => PerformanceNormal(i.toDouble)
          }
        }
        val (ioId, seed) = extra match {
          case extraInfo(ioIdS, seedS) => (ioIdS.toInt, seedS.toInt)
        }
        val date = TimeTools.numericalDateTimeFormat.parse(dateS)
        ResultInfo(name, perf, ioId, seed, date, dirName)
    }
  }

  val problemMap = TextbookExamples.allProblems.groupBy(_.problemName)

  def showInfo(info: ResultInfo) = {
    s"""[ioId=${info.ioId},performance=${info.performance}]"""
  }

  def analyze(resultsPath: Path) = {

    val sizeOfInterest = 250

    val tasks =
      (ls ! resultsPath).filter { p =>
        p.fileType == FileType.Dir
      }.map { p =>
        parseResultInfo(p.name)
      }.groupBy(_.name).mapValues { results =>
        results.sortBy(info => performanceToDouble(info.performance)).reverse.toIndexedSeq
      }.toIndexedSeq.sortBy(pair => performanceToDouble(pair._2.head.performance)).reverse

    println(s"${tasks.size} tasks in total.")
    for (((name, results)) <- tasks if problemMap.contains(name)){
      println(s"--- Task: $name ---")
      println(s"number of results: ${results.length}")

      val problem = problemMap(name).head
      results.take(5).foreach{ info =>

        info.performance match{
          case PerformanceNormal(_) =>
            val indFilePath = resultsPath / info.dirName / "bestIndividual.serialized"
            val ind = FileInteraction.readMultiIndFromFile(indFilePath.toString(), StandardSystem.funcMap)
            val evaluator = {
              import problem._
              new FittingPerformanceEvaluation(sizeOfInterest, resourceUsage, sizeF, breakingMemoryUsage = sizeOfInterest * 500, nonsenseFitness = -1, minPointsToUse = 8, maxPointsToUse = 50,
                fitter = PowerLawFitter(maxIter = 300), modelToScore = FittingPerformanceEvaluation.pModel)
            }
            val inputStream = MultiStateRepresentation.individualToPattern(ind)
            val eval = evaluator.evaluateAPattern(inputStream)
            println(showInfo(info)+ " -> " + eval)
          case _ =>
            println(showInfo(info))
        }

      }
      println(s"worst: ${showInfo(results.last)}")
    }
  }

  def analyzeSingularityResults(resultsPath: Path) = {
      val infos =  (ls ! resultsPath).filter { p =>
        p.fileType == FileType.Dir
      }.map { p =>
        parseResultInfo(p.name)
      }.groupBy(_.name).mapValues { results =>
        results.sortBy(info => performanceToDouble(info.performance)).reverse.toIndexedSeq.head
      }.toIndexedSeq
      val sizeExtrat = """(.+)_int_(.+)""".r
      infos.map{ case (name, info) =>
        name match {
          case sizeExtrat(n, size) =>
            val s = if(n.endsWith("sort")||n.toLowerCase()=="phphash") size.toInt *4 else size.toInt
            (n, s, info.performance.asInstanceOf[PerformanceNormal].value)
        }
      }.sortBy(x => (x._1, x._2)).foreach{
        x => println{
          IS(x._1, x._2, x._3).mkString(", ")
        }
      }
  }

  private val indWithDateRegex = """bestIndividual\[time=(.+)\].serialized""".r
  def getLatestInd(dir: Path): Path = {
    (ls ! dir).map{ p =>
      p.name match {
        case indWithDateRegex(time) => (time.toLong, p)
      }
    }.maxBy(_._1)._2
  }

  def analyzeOneResult(resultDir: Path, sizeOfInterest: Int) = {
    val info = parseResultInfo(resultDir.name)
    val problem = problemMap(info.name).head

    info.performance match {
      case PerformanceNormal(_) =>
        val indFilePath = resultDir / "bestIndividual.serialized"
        val ind = FileInteraction.readMultiIndFromFile(indFilePath.toString(), StandardSystem.funcMap)
        val evaluator = {
          import problem._
          new FittingPerformanceEvaluation(sizeOfInterest, resourceUsage, sizeF, breakingMemoryUsage = sizeOfInterest * 500, nonsenseFitness = -1, minPointsToUse = 8, maxPointsToUse = 200,
            fitter = PowerLawFitter(maxIter = 300), modelToScore = FittingPerformanceEvaluation.pModel)
        }
        val inputStream = MultiStateRepresentation.individualToPattern(ind)
        val eval = evaluator.evaluateAPattern(inputStream)
        println(showInfo(info) + " -> " + eval)
        val fittingData = eval.info.split("data = ").last

        val fitterFile = pwd / "mathematica" / "FitterData.txt"
        rm(fitterFile)
        write(fitterFile, fittingData)
      case _ =>
        println(showInfo(info))
    }
  }

  def analyzeOneGraphResult(resultDir: Path, sizeOfInterest: Int, graphToSize: GraphValue => Double) = {
    val info = parseResultInfo(resultDir.name)
    val problem = problemMap(info.name).head

    info.performance match {
      case PerformanceNormal(_) =>
        val indFilePath = resultDir / "bestIndividual.serialized"
        val ind = FileInteraction.readMultiIndFromFile(indFilePath.toString(), StandardSystem.funcMap)
        val evaluator = {
          import problem._
          new FittingPerformanceEvaluation(sizeOfInterest, resourceUsage, sizeF, breakingMemoryUsage = sizeOfInterest * 500, nonsenseFitness = -1, minPointsToUse = 8, maxPointsToUse = 100,
            fitter = PowerLawFitter(maxIter = 300), modelToScore = FittingPerformanceEvaluation.pModel,
            extraSizeF = Some(xs => xs.collect{ case gv: GraphValue => graphToSize(gv)}.sum))
        }
        val inputStream = MultiStateRepresentation.individualToPattern(ind)
        val eval = evaluator.evaluateAPattern(inputStream)
        println(showInfo(info) + " -> " + eval)
        val fittingData = eval.info.split("data = ").last

        val fitterFile = pwd / "mathematica" / "FitterData.txt"
        rm(fitterFile)
        write(fitterFile, fittingData)
      case _ =>
        println(showInfo(info))
    }
  }


  def main(args: Array[String]): Unit = {
    analyze(Path.home / "Downloads" / "experiment1")

//    analyzeOneGraphResult(Path.home / "Downloads" / "experiment1" / "textbook.hopcroftKarpBiMatch[performance=79103.0][ioId=1043,seed=1043](18-02-28-12/01/47)".replace("/",":"),
//      sizeOfInterest = 250*8,
//      graphToSize = { gv =>
//        import math.log
//        val e = gv.edges.length
//        val v = gv.nodeNum
//        e * math.sqrt(v)
//      })

//    analyzeSingularityResults(Path.home / "Downloads" / "singularity_results")
  }
}
