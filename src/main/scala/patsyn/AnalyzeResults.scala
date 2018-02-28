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

  def analyze(resultsPath: Path) = {
    def showInfo(info: ResultInfo) = {
      s"""[ioId=${info.ioId},performance=${info.performance}]"""
    }

    val problemMap = TextbookExamples.allProblems.groupBy(_.problemName)
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
      results.take(5).zipWithIndex.foreach{ case(info, i) =>

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


  def main(args: Array[String]): Unit = {
    analyze(Path.home / "Downloads" / "experiment1")
  }
}
