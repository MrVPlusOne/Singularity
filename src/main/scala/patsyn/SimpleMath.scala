package patsyn

object SimpleMath {
  def wrapInRange(i: Int, range: Int): Int = {
    val m = i % range
    if (m < 0) m + range else m
  }

  def square(x: Double): Double = x * x
}
