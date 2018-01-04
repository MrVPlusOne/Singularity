package benchmarks

import patsyn.ProblemConfig
import patsyn.StandardSystem.AccountingWizard.{EmptyCommand, OrderCommand, UpdateHours}
import patsyn._
import patsyn.StandardSystem._
import edu.utexas.stac.Cost

object AccountingWizardExample {

  def cmdToJson(cmdValue: EValue) = cmdValue match {
    case OrderCommand(itemName, cost, quantity) =>
      s"""{"itemName":"$itemName","cost":$cost,"quantity":$quantity}"""
    case UpdateHours(quatity) =>
      s"""{"quantity":"$quatity"}"""
    case _:EmptyCommand =>
      ""
  }

  def timingExample(workingDir: String): ProblemConfig = {

    ProblemConfig(
      "stac.e5.accountingwizard.timing",
      outputTypes = IS(EVect(AccountingWizard.Command), EVect(EInt), EInt),
      sizeF = {
        case IS(VectValue(commands), VectValue(pname), IntValue(cap)) =>
          val setupSize = 250 + pname.length
          val cmdSize = commands.map(c => cmdToJson(c).length + 250).sum
          setupSize + cmdSize
      },
      resourceUsage = {
        case IS(VectValue(commands), VectValue(pname), IntValue(cap)) =>
          ???
      }
    )
  }

  def main(args: Array[String]): Unit = {
    Cost.reset()
    com.bbn.accounting.wizard.AccountingWizard.main(Array())
    println(s"HAHA1 ${Cost.read()}")
    spark.Spark.stop()
    Cost.reset()
    com.bbn.accounting.wizard.AccountingWizard.main(Array())
    println(s"HAHA2 ${Cost.read()}")
    spark.Spark.stop()
  }
}
