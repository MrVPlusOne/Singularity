package patsyn


import edu.utexas.stac.Cost
import patbench.blogger.fi.iki.elonen.JavaWebServer


object TestCostInstr {
  import sys.process._

  def javaWebServerExample(): Unit ={
    new Thread(() => JavaWebServer.main(Array())).start()

    "curl -i http://localhost:8080/\r11111111111111111111111" !
  }

  def imageClassification(): Unit ={
//    import com.stac.Main
//    Main.main(Array("cluster", "input-files/bad-image.png"))
  }

  def main(args: Array[String]): Unit = {

    imageClassification()

//    CommandProcessor.main("dot input-files/small-graph.dot xy diagram png output-files/PNG_output.png".split(" "))
    val cost = Cost.read()
    println(cost)
    System.exit(0)
  }

}
