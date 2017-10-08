package patsyn

import regex._
//import java.util.regex._

object RegexStatistics {

  def main(args: Array[String]): Unit = {
    val c = new Counter()
    println{
      Pattern.compile("(abc)+").matcher(c, "abc"*100).find()
    }
    println(s"Count: ${c.read()}")
  }

}
