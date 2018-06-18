package patsyn

import com.wolfram.jlink._
import patsyn.StandardSystem.GraphValue

class MamLink private(initCode: String) {
  var logInteraction = false

  val macOptions = """-linkmode launch -linkname '"/Applications/Mathematica.app/Contents/MacOS/MathKernel" -mathlink'"""
  val link = MathLinkFactory.createKernelLink("-linkmode launch -linkname '\"/Applications/Mathematica.app/Contents/MacOS/MathKernel\" -mathlink'")
  link.discardAnswer()

  link.evaluate(initCode)
  link.discardAnswer()

  def execute(expr: String): String = {
    if(logInteraction){
      println(s"[MathLink Interaction]")
      println(s"Input:")
      println(expr)
    }
    link.evaluate(expr)
    link.waitForAnswer()
    val r = link.getExpr.toString
    if(logInteraction){
      println(s"Result:")
      println(r)
      println("[End of MathLink Interaction]")
    }
    r
  }

  private def close() = link.close()
}

object MamLink{
  def runWithALink[T](initCode: String)(f: MamLink => T): T ={
    val link = new MamLink(initCode)
    try {
      f(link)
    }finally {
      link.close()
    }
  }

  def runWithALinkOnMac[T](f: MamLink => T): T = {
    runWithALink("")(f)
  }
}

object MamFormat{
  def showDouble(v: Double, precision: Int = 6): String = {
    f"$v%.6e".replace("e+","*10^").replace("e-","*10^-")
  }

  def showAsMamGraph(graph: GraphValue): String = {
    val vertexList = (0 until graph.nodeNum).map(i => s"Labeled[$i, Style[$i,Red]]").mkString("{",",","}")
    val edgeList = graph.edges.map{
      case (from, to, value) => s"""Labeled[$from -> $to,Tooltip["$value"]]"""
    }.mkString("{",",","}")
    s"""Graph[$vertexList, $edgeList]"""
  }
}
