package patsyn

object Debug {
  def log[A](name: String)(v: => A): A ={
    println(s"[$name begin]")
    val r = v
    println(r)
    println(s"[$name end]")
    r
  }

  def logLn[A](s: String): Unit ={
    println(s)
  }
}
