package patsyn


object Evaluation {

  def gaussianSquared(halfPoint: Double)(x: Double): Double = {
    math.pow(2,-math.pow(x/halfPoint,4))
  }
}

trait Evaluation {
  def evaluateAPattern(resourceUsage: IS[EValue] => Double, sizeF: IS[EValue] => Int)
                      (seeds: IS[Expr], iters: IS[Expr]): (Double, Stream[IS[EValue]])
}

object ExtrapolateEvaluation{
  sealed trait EvaluationResult{
    def value: Double
  }

  case class ExtrapolatedResult(value: Double, dataPoints: Seq[(Int, Double)]) extends EvaluationResult
  case class MaxSoFarResult(value: Double) extends EvaluationResult


  def main(args: Array[String]): Unit = {
    MamLink.runWithALinkOnMac{ link =>
      link.logInteraction = true
      val eval = new ExtrapolateEvaluation(link, sizeOfInterest = 300, fitEndPoint = 50, minPointsToUse = 9)
      val xs = Stream.iterate(1)(_ + 1).map(x => x*x)
      var timesExecuted = 0
      val result = eval.evaluate[Int](xs, x => {timesExecuted+=1; x}, x => math.pow(x.toDouble,2))
      println(result)
      println(s"Times Executed: $timesExecuted")
    }
  }
}

class ExtrapolateEvaluation(mamLink: MamLink, sizeOfInterest: Int, fitEndPoint: Int, minPointsToUse: Int, nonsenseFitness: Double = 0.0) extends Evaluation {
  import ExtrapolateEvaluation._

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

  def evaluateAPattern(resourceUsage: IS[EValue] => Double, sizeF: IS[EValue] => Int)
                      (seeds: IS[Expr], iters: IS[Expr]): (Double, Stream[IS[EValue]]) = {
    import patsyn.StandardSystem.IntValue

    var index = 0
    val seedValues = seeds.map(seed => Expr.evaluateWithCheck(seed, IS()))
    val inputStream = Stream.iterate(seedValues)(ls => {
      val lsWithIdx = IntValue(index) +: ls
      val ls1 = iters.map { iter => Expr.evaluateWithCheck(iter, lsWithIdx) }
      if (sizeF(ls1) <= sizeF(ls))
        return (nonsenseFitness, Stream())
      index += 1
      ls1
    })

    val extrapolationValue = this.evaluate[IS[EValue]](inputStream, sizeF, resourceUsage).value

    (extrapolationValue, inputStream)
  }
}

class SimpleEvaluation(sizeOfInterest: Int, maxTrials: Int, nonsenseFitness: Double = 0.0) extends Evaluation {
  def evaluateAPattern(resourceUsage: (IS[EValue]) => Double, sizeF: (IS[EValue]) => Int)
                      (seeds: IS[Expr], iters: IS[Expr]): (Double, Stream[IS[EValue]]) = {
    import patsyn.StandardSystem.IntValue

    var index = 0
    val seedValues = seeds.map(seed => Expr.evaluateWithCheck(seed, IS()))
    val inputStream = Stream.iterate(seedValues)(ls => {
      val lsWithIdx = IntValue(index) +: ls
      val ls1 = iters.map { iter => Expr.evaluateWithCheck(iter, lsWithIdx) }
      if (sizeF(ls1) <= sizeF(ls))
        return (nonsenseFitness, Stream())
      index += 1
      ls1
    })

    val pointsToTry = inputStream.takeWhile(input => sizeF(input) <= sizeOfInterest).takeRight(maxTrials)
    (pointsToTry.map(resourceUsage).max, inputStream)
  }
}



