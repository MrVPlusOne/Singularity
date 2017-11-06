package patsyn

import java.io.FileWriter

import user.commands.CommandProcessor
import edu.utexas.stac.Cost

object TestCostInstr {

  def main(args: Array[String]): Unit = {
//
//    val fileContent = FuzzingExample.intVectorToGraph(0, Vector(1, 2, 3, 0,1))
//    val dotFile = "input-files/genGraph.dot"
//    val fw = new FileWriter(dotFile)
//    fw.write(fileContent)
//    fw.close()

    CommandProcessor.main("dot input-files/small-graph.dot xy diagram png output-files/PNG_output.png".split(" "))
    val cost = Cost.read()
    Cost.inc()
    println(cost)
  }

}
