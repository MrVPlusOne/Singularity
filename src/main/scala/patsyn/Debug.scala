package patsyn

object Debug {
  def debug[A](name: String)(v: => A): A ={
    println(s"[$name begin]")
    val r = v
    println(s"[$name end]")
    r
  }
}
