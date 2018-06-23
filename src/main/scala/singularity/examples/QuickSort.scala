package singularity.examples

import singularity.Runner.RunnerConfig
import singularity.StandardSystem._
import singularity._

import scala.util.Random

/** A minimal example to use Singularity */
object QuickSort {

  def pivotChoose(xs: IS[Int]) = xs(xs.length/2) //can also try "xs.head", "xs.tail" etc..

  var counter = 0

  def quickSort(xs: IS[Int]): IS[Int] = {
    counter += 1
    if(xs.length < 2) return xs

    val pivot = pivotChoose(xs)
    val left = xs.filter(_ < pivot)
    val right = xs.filter(_ > pivot)
    val middle = xs.filter(_ == pivot)
    counter += xs.length
    quickSort(left) ++ middle ++ quickSort(right)
  }

  val quickSortProblem = ProblemConfig(
    problemName = "quickSort example",
    outputTypes = IS(EVect(EInt)),
    resourceUsage = {
      case IS(VectValue(vs)) =>
        val xs = vs.map(_.asInstanceOf[IntValue].value)
        counter = 0
        quickSort(xs)
        counter
    },
    sizeF = {
      case IS(VectValue(vs)) => vs.length
    }
  )

  def main(args: Array[String]): Unit = {
    val randomSeed = 0
    val fuzzingSize = 100
    val sizePolicy = FixedEvalSize(fuzzingSize)
    val runnerConfig = RunnerConfig().copy(randomSeed = randomSeed, useGUI = true)

    Supernova.standardSupernova.fuzzProblem(
      quickSortProblem,
      runnerConfig,
      execConfig = ExecutionConfig().copy(evalSizePolicy = sizePolicy),
      rand = new Random(randomSeed))
  }
}
