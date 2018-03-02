package patsyn

import java.util.Date

import measure.TimeTools
import ammonite.ops._
import benchmarks.TextbookExamples
import patsyn.FittingPerformanceEvaluation.PowerLawFitter

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
    tasks.foreach{ case (name, results) =>
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

  def main(args: Array[String]): Unit = {
    analyzeOneResult(Path.home / "Downloads" / "experiment1" / "textbook.hopcroftKarpBiMatch[performance=76175.0][ioId=1045,seed=1045](18-02-28-12:10:48)", sizeOfInterest = 250*8*8)

//    analyze(Path.home / "Downloads" / "experiment1")
  }
}
