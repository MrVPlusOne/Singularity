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

  def aggressiveSigmoid(aggressiveness: Double) = (x: Double) => {
    val a = math.pow(50, 2*(0.5 - aggressiveness))
    math.pow(x,a)
  }

  def aggressiveInterpolate(aggressiveness: Double, from: Double, to: Double)(x: Double) = {
    linearInterpolate(from, to)(aggressiveSigmoid(aggressiveness)(x))
  }

  def linearInterpolate(from: Double, to: Double)(x: Double): Double = {
    (to - from) * x + from
  }

  def expInterpolate(from: Double, to: Double)(x: Double): Double = {
    val ratio = to/from
    from*math.pow(ratio, x)
  }

  def sigmoid(x: Double) = 1.0/(1+math.exp(-x))

  def parallelMap[A,B](seq: IS[A], f: A => B, threadNum: Int): IS[B] = {
    import scala.collection.parallel
    import parallel._

    val taskSupport = new ForkJoinTaskSupport(new scala.concurrent.forkjoin.ForkJoinPool(threadNum))

    if(threadNum>1) {
      val p = seq.par
      p.tasksupport = taskSupport
      p.map(f).toIndexedSeq
    }else{
      seq.map(f).toIndexedSeq
    }
  }
}
