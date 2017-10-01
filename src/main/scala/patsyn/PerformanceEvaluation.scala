package patsyn


object PerformanceEvaluation {
}

trait PerformanceEvaluation {
  def resourceUsage: IS[EValue] => Double
  def sizeF: IS[EValue] => Int
  def nonsenseFitness: Double

  def evaluateAPattern(inputStream: Stream[IS[EValue]]): Double
}

object ExtrapolatePerformanceEvaluation{
  sealed trait EvaluationResult{
    def value: Double
  }

  case class ExtrapolatedResult(value: Double, dataPoints: Seq[(Int, Double)]) extends EvaluationResult
  case class MaxSoFarResult(value: Double) extends EvaluationResult

}

class ExtrapolatePerformanceEvaluation(mamLink: MamLink, sizeOfInterest: Int, fitEndPoint: Int, minPointsToUse: Int, val resourceUsage: IS[EValue] => Double, val sizeF: IS[EValue] => Int, val nonsenseFitness: Double = 0.0) extends PerformanceEvaluation {
  import ExtrapolatePerformanceEvaluation._

  def evaluate[T](xs: Stream[T], sizeF: T => Int, executeF: T => Double): EvaluationResult = {
    def maxSmooth(data: Seq[Double]): Seq[Double] = {
      data.scanLeft(nonsenseFitness)(math.max).tail
    }

    def fitData(xyData: Seq[(Int, Double)]): ExtrapolatedResult = {
      val maxSmoothed = xyData.map(_._1) zip maxSmooth(xyData.map(_._2))
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


  def evaluateAPattern(inputStream: Stream[IS[EValue]]): Double = {
    var lastSize = Int.MinValue
    val xs = inputStream.map{ input =>
      val inputSize = sizeF(input)
      if(inputSize <= lastSize)
        return nonsenseFitness
      lastSize = inputSize
      input
    }
    this.evaluate[IS[EValue]](xs, sizeF, resourceUsage).value
  }

}

class SimplePerformanceEvaluation(sizeOfInterest: Int, maxTrials: Int, val resourceUsage: (IS[EValue]) => Double, val sizeF: (IS[EValue]) => Int, val nonsenseFitness: Double = 0.0) extends PerformanceEvaluation {


  def evaluateAPattern(inputStream: Stream[IS[EValue]]): Double = {
    var lastSize = Int.MinValue
    val pointsToTry = inputStream.takeWhile{input =>
      val inputSize = sizeF(input)
      if(inputSize <= lastSize)
        return nonsenseFitness
      lastSize = inputSize
      sizeF(input) <= sizeOfInterest
    }.takeRight(maxTrials)
    pointsToTry.map(resourceUsage).max
  }
}



