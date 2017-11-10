package patsyn

import scala.util.Random

object SimpleMath {
  def wrapInRange(i: Int, range: Int): Int = {
    val m = i % range
    if (m < 0) m + range else m
  }

  def square(x: Double): Double = x * x

  def gaussianForthOrder(halfPoint: Double)(x: Double): Double = {
    math.pow(2,-math.pow(x/halfPoint,4))
  }

  def randomSelect[A](random: Random)(xs: IndexedSeq[A]): A = {
    val i = random.nextInt(xs.length)
    xs(i)
  }

  def natToList(n: Int, base: Int): List[Int] = {
    require(n>=0)
    require(base > 0)
    def rec(n: Int): List[Int] = {
      if(n==0) List()
      else{
        val residual = n % base
        residual :: rec(n/base)
      }
    }
    val r = rec(n).reverse
    if(r.isEmpty) List(0) else r
  }
}
