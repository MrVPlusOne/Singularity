package patsyn

import org.apache.commons.math3.analysis.ParametricUnivariateFunction
import org.apache.commons.math3.exception.ConvergenceException
import patsyn.EvolutionRepresentation.MemoryUsage

import scala.collection.mutable
import scala.util.Random


object PerformanceEvaluation {
  case class EvaluationResult(value: Double, extraData: AnyRef)
}

case class PerformanceEvalResult(perf: Double, info: String)

trait PerformanceEvaluation {
  def resourceUsage: IS[EValue] => Double
  def sizeF: IS[EValue] => Int
  def nonsenseFitness: Double
  def breakingMemoryUsage: Long

  def evaluateAPattern(inputStream: Stream[(MemoryUsage, IS[EValue])]): PerformanceEvalResult
}

object ExtrapolatePerformanceEvaluation{
  sealed trait EvaluationResult{
    def value: Double
  }

  case class ExtrapolatedResult(value: Double, dataPoints: Seq[(Int, Double)]) extends EvaluationResult
  case class MaxSoFarResult(value: Double) extends EvaluationResult

}


class SimplePerformanceEvaluation(sizeOfInterest: Int, evaluationTrials: Int, val resourceUsage: (IS[EValue]) => Double, val sizeF: (IS[EValue]) => Int, val breakingMemoryUsage: Long, val nonsenseFitness: Double) extends PerformanceEvaluation {


  def evaluateAPattern(inputStream: Stream[(MemoryUsage, IS[EValue])]): PerformanceEvalResult = {
    var lastSize = Int.MinValue
    val pointsToTry = inputStream.takeWhile { case (usage, input) =>
      val inputSize = sizeF(input)
      if (inputSize <= lastSize || usage.amount > breakingMemoryUsage)
        return PerformanceEvalResult(nonsenseFitness, "inputSize <= lastSize || usage.amount > breakingMemoryUsage")
      lastSize = inputSize
      sizeF(input) <= sizeOfInterest
    }.takeRight(evaluationTrials)

    if (pointsToTry.isEmpty)
      PerformanceEvalResult(nonsenseFitness, "pointsToTry.isEmpty")
    else
      PerformanceEvalResult(pointsToTry.map(_._2).map(resourceUsage).max, "Simple Eval")
  }
}

object FittingPerformanceEvaluation {

  trait ModelFitter{
    def fitModel(xyPoints: IS[(Double, Double)], xRange: (Double, Double)): ((Double => Double), Double, String)
  }

  case class PowerLawFitter(maxIter: Int) extends ModelFitter{

    def fitModel(xyPoints: IS[(Double, Double)], xRange: (Double, Double)): (Double => Double, Double, String) = {
      import collection.JavaConverters._
      import org.apache.commons.math3.fitting.{SimpleCurveFitter, WeightedObservedPoints}
      val obPoints = new WeightedObservedPoints()
      val nPoints = xyPoints.length
      val xRangeSize = xRange._2 - xRange._1
      xyPoints.indices.foreach{i =>
        val d1 = if(i>0) xyPoints(i)._1 - xyPoints(i-1)._1 else 0.0
        val d2 = if(i+1<nPoints) xyPoints(i+1)._1 - xyPoints(i)._1 else 0.0
        val weight = (d1+d2)/xRangeSize
        obPoints.add(weight, xyPoints(i)._1, xyPoints(i)._2)
      }

      val observations = obPoints.toList
      val Array(a,b,c) = ModifiedCurveFitter.create(PowerLawModel, Array(10.0, 2.0, 0.0)).withMaxIterations(maxIter).fit(observations)
      def f(x: Double) = a * math.pow(x, b) + c

      val weights = new Array[Double](nPoints)
      val xs = new Array[Double](nPoints)
      val ys = new Array[Double](nPoints)
      observations.asScala.zipWithIndex.foreach{
        case (wp, i) =>
          weights(i) = wp.getWeight
          xs(i) = wp.getX
          ys(i) = wp.getY
      }
      val beta: Double = {

        val rs = SimpleMath.rSquared(xs, ys, xs.map(f), weights)
        assert(0.0 <= rs && rs <= 1.0)
        10.0 * math.pow(10, -1.0/rs)
      }

      (f, beta, s"$a * x ^ $b + $c, beta = $beta, data = {${xs.mkString("{",",","}")},${ys.mkString("{",",","}")}}")
    }
  }

  object PowerLawModel extends ParametricUnivariateFunction{
    def gradient(x: Double, abc: Double*): Array[Double] = {
      val Seq(a,b,c) = abc
      val xb = math.pow(x, b)
      Array(xb, a * xb * math.log(x),1.0)
    }

    def value(x: Double, abc: Double*): Double = {
      val Seq(a,b,c) = abc
      a * math.pow(x, b) + c
    }
  }

}

class FittingPerformanceEvaluation(sizeOfInterest: Int, val resourceUsage: (IS[EValue]) => Double,
                                   val sizeF: (IS[EValue]) => Int,
                                   val breakingMemoryUsage: Long,
                                   val nonsenseFitness: Double,
                                   val minPointsToUse: Int,
                                   val maxPointsToUse: Int,
                                   val fitter: FittingPerformanceEvaluation.ModelFitter
                                   ) extends PerformanceEvaluation {

  def usableInputs(inputStream: Stream[(MemoryUsage, IS[EValue])]): Option[IS[(Int, IS[EValue])]] = {
    var lastSize = Int.MinValue
    val xyPoints = mutable.ListBuffer[(Int, IS[EValue])]()
    inputStream.foreach{ case (usage, input) =>
      val inputSize = sizeF(input)
      if (inputSize <= lastSize || usage.amount > breakingMemoryUsage)
        return None
      lastSize = inputSize
      val shouldContinue = sizeF(input) <= sizeOfInterest
      if(!shouldContinue)
        return Some(xyPoints.toIndexedSeq)
      xyPoints.append(inputSize -> input)
    }
    Some(xyPoints.toIndexedSeq)
  }


  def evaluateAPattern(inputStream: Stream[(MemoryUsage, IS[EValue])]): PerformanceEvalResult = {

    usableInputs(inputStream) match {
      case Some(pts) =>
        val points = pts.filter(p => p._1 > 0)
        if (points.length < minPointsToUse)
          return PerformanceEvalResult(nonsenseFitness, "points.length < minPointsToUse")
        val xyPoints = {
          val ps = SimpleMath.randomSelectFrom(points, maxPointsToUse, new Random(1)).map{
            case (x, input) => x.toDouble -> resourceUsage(input)
          }
          ps.map(_._1) zip SimpleMath.maxSmooth(ps.map(_._2))
        }
        val xRange = (xyPoints.head._1, sizeOfInterest.toDouble)
        try {
          val (model, beta, info) = fitter.fitModel(xyPoints, xRange)
          PerformanceEvalResult(model(sizeOfInterest) * beta, info)
        } catch {
          case cE: ConvergenceException =>
            System.err.println{"ConvergenceException when try to fit a curve during resource usage evaluation."}
            System.err.println{s"xyPoints: $xyPoints"}
            System.err.println{s"xRange: $xRange"}
            throw cE
        }
      case _ => PerformanceEvalResult(nonsenseFitness, "usableInputs is None")
    }
  }
}


