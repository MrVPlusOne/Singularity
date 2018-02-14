package patsyn

import patbench.commons.math3.analysis.ParametricUnivariateFunction
import patsyn.EvolutionRepresentation.MemoryUsage

import scala.collection.mutable


object PerformanceEvaluation {
}

trait PerformanceEvaluation {
  def resourceUsage: IS[EValue] => Double
  def sizeF: IS[EValue] => Int
  def nonsenseFitness: Double
  def breakingMemoryUsage: Long

  def evaluateAPattern(inputStream: Stream[(MemoryUsage, IS[EValue])]): Double
}

object ExtrapolatePerformanceEvaluation{
  sealed trait EvaluationResult{
    def value: Double
  }

  case class ExtrapolatedResult(value: Double, dataPoints: Seq[(Int, Double)]) extends EvaluationResult
  case class MaxSoFarResult(value: Double) extends EvaluationResult

}

@deprecated
class ExtrapolatePerformanceEvaluation(mamLink: MamLink, sizeOfInterest: Int, fitEndPoint: Int, minPointsToUse: Int, val resourceUsage: IS[EValue] => Double, val sizeF: IS[EValue] => Int, val nonsenseFitness: Double = 0.0) extends PerformanceEvaluation {
  import ExtrapolatePerformanceEvaluation._

  def breakingMemoryUsage = ???

  def evaluate[T](xs: Stream[T], sizeF: T => Int, executeF: T => Double): EvaluationResult = {

    def fitData(xyData: Seq[(Int, Double)]): ExtrapolatedResult = {
      val maxSmoothed = xyData.map(_._1) zip SimpleMath.maxSmooth(xyData.map(_._2))
      val smoothData = maxSmoothed.filterNot(p => p._1 == 0 && p._2 == 0.0) //hacking fix
      val dataString = smoothData.map{case (x,y) => s"{$x,${MamFormat.showDouble(y)}}"}.mkString("{",",","}")
      val mamCode = s"FindFit[$dataString, a x^b + c, {{a, 1.0}, {b, 1.1}, {c, 1.1}}, x][[All, 2]]"
      val result = mamLink.execute(mamCode)
      assert(result.head == '{' && result.last == '}', s"Invalid result: $result")
      val Array(a,b,c) = result.substring(1,result.length-1).split(",").map(s => s.replace("*^","e").toDouble)

      ExtrapolatedResult(a * math.pow(sizeOfInterest, b) + c, xyData)
    }

    val sizedInputs = xs.map(x => (sizeF(x), x))
    val smallInputs = sizedInputs.takeWhile{ _._1 <= fitEndPoint }
    if(smallInputs.size >= minPointsToUse)
      return fitData(smallInputs.map{case (size, v) => size -> executeF(v)})


    var i = 0
    val largerDataSet = sizedInputs.takeWhile{
      case (size, v) =>
        i += 1
        size <= sizeOfInterest && i <= minPointsToUse
    }
    if(largerDataSet.size < minPointsToUse)
      MaxSoFarResult(largerDataSet.map{p => executeF(p._2)}.max)
    else
      fitData(largerDataSet.map{case (size, v) => size -> executeF(v)})


  }


  def evaluateAPattern(inputStream: Stream[(MemoryUsage, IS[EValue])]): Double = {
//    var lastSize = Int.MinValue
//    val xs = inputStream.map{ input =>
//      val inputSize = sizeF(input)
//      if(inputSize <= lastSize)
//        return nonsenseFitness
//      lastSize = inputSize
//      input
//    }
//    this.evaluate[IS[EValue]](xs, sizeF, resourceUsage).value
    ???
  }

}

class SimplePerformanceEvaluation(sizeOfInterest: Int, evaluationTrials: Int, val resourceUsage: (IS[EValue]) => Double, val sizeF: (IS[EValue]) => Int, val breakingMemoryUsage: Long, val nonsenseFitness: Double) extends PerformanceEvaluation {


  def evaluateAPattern(inputStream: Stream[(MemoryUsage, IS[EValue])]): Double = {
    var lastSize = Int.MinValue
    val pointsToTry = inputStream.takeWhile { case (usage, input) =>
      val inputSize = sizeF(input)
      if (inputSize <= lastSize || usage.amount > breakingMemoryUsage)
        return nonsenseFitness
      lastSize = inputSize
      sizeF(input) <= sizeOfInterest
    }.takeRight(evaluationTrials)

    if (pointsToTry.isEmpty)
      nonsenseFitness
    else
      pointsToTry.map(_._2).map(resourceUsage).max
  }
}

object FittingPerformanceEvaluation {
  trait ModelFitter{
    def fitModel(xyPoints: IS[(Double, Double)], xRange: (Double, Double)): ((Double => Double), Double)
  }

  case class PowerLawFitter(maxIter: Int) extends ModelFitter{

    def fitModel(xyPoints: IS[(Double, Double)], xRange: (Double, Double)): (Double => Double, Double) = {
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
      val Array(a,b) = new SimpleCurveFitter(PowerLawModel, Array(10.0, 2.0), maxIter).fit(observations)
      def f(x: Double) = a * math.pow(x, b)

      val beta: Double = {
        val weights = new Array[Double](nPoints)
        val xs = new Array[Double](nPoints)
        val ys = new Array[Double](nPoints)
        observations.asScala.zipWithIndex.foreach{
          case (wp, i) =>
            weights(i) = wp.getWeight
            xs(i) = wp.getX
            ys(i) = wp.getY
        }
        SimpleMath.rSquared(xs, ys, xs.map(f), weights)
      }
      (f, beta)
    }
  }

  object PowerLawModel extends ParametricUnivariateFunction{
    def gradient(x: Double, ab: Double*): Array[Double] = {
      val Seq(a,b) = ab
      val xb = math.pow(x, b)
      Array(xb, a * xb * math.log(x))
    }

    def value(x: Double, ab: Double*): Double = {
      val Seq(a,b) = ab
      a * math.pow(x, b)
    }
  }

}

class FittingPerformanceEvaluation(sizeOfInterest: Int, val resourceUsage: (IS[EValue]) => Double,
                                   val sizeF: (IS[EValue]) => Int,
                                   val breakingMemoryUsage: Long,
                                   val nonsenseFitness: Double,
                                   val minPointsToUse: Int,
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


  def evaluateAPattern(inputStream: Stream[(MemoryUsage, IS[EValue])]): Double = {

    usableInputs(inputStream) match {
      case Some(points) if points.length >= minPointsToUse =>
        val xyPoints = {
          val ps = points.map{
            case (x, input) => x.toDouble -> resourceUsage(input)
          }
          ps.map(_._1) zip SimpleMath.maxSmooth(ps.map(_._2))
        }
        val xRange = (xyPoints.head._1, sizeOfInterest.toDouble)
        val (model, beta) = fitter.fitModel(xyPoints, xRange)
        model(sizeOfInterest) * beta
      case _ => nonsenseFitness
    }
  }
}


