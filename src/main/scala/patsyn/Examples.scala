package patsyn

import scala.util.Random

class Counter{

  private var counter = 0L

  def count(n: Int = 1): Unit = {
    counter += n
  }

  def read(): Double = {
    counter.toDouble
  }
}



object Examples {

  def quickSort(c: Counter, pivotChoose: (IS[Int]) => Int)(xs: IS[Int]): IS[Int] = {
    c.count()
    if(xs.length < 2) return xs

    val pivot = pivotChoose(xs)
    val left = xs.filter(_ < pivot)
    val right = xs.filter(_ > pivot)
    val middle = xs.filter(_ == pivot)
    c.count(xs.length)
    quickSort(c, pivotChoose)(left) ++ middle ++ quickSort(c, pivotChoose)(right)
  }

  def insertionSort(c: Counter)(xs: IS[Int]): IS[Int] = {
    val array = xs.toArray

    def sortPart(n: Int): Unit = {
      val x = array(n)
      for(i <- n-1 to 0 by -1){
        c.count(2)
        array(i+1) = array(i)

        c.count()
        if(array(i)<=x){
          c.count()
          array(i) = x
          return
        }
      }
      c.count()
      array(0) = x
    }
    for(i <- xs.indices){
      c.count()
      sortPart(i)
    }
    array
  }

  def listSearchAndCopy(c: Counter)(xs: IS[Int], e: Int): Option[IS[Int]] = {
    for(i <- xs.indices){
      c.count(2)
      if(xs(i) == e){
        c.count(i+1)
        return Some(xs.take(i))
      }
    }
    c.count()
    None
  }



  def main(args: Array[String]): Unit = {
    val c = new Counter()
    val rand = new Random(1)
    val r = quickSort(c, _.last)(0 until 20)
    println(r)
    println(c.read())
  }
}
