package patsyn


import user.commands.CommandProcessor
import edu.utexas.stac.Cost
import fi.iki.elonen.{JavaWebServer, NanoHTTPD$IHTTPSession}


object TestCostInstr {
  import sys.process._

  def main(args: Array[String]): Unit = {

    new Thread(() => JavaWebServer.main(args)).start()

    "curl -i http://localhost:8080/\r11111111111111111111111" !

//    CommandProcessor.main("dot input-files/small-graph.dot xy diagram png output-files/PNG_output.png".split(" "))
    val cost = Cost.read()
    println(cost)
    System.exit(0)
  }

}
