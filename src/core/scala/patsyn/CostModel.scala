package patsyn

object CostModel {
  def exprCost(expr: Expr): Double = expr match {
    case _: EArg => 0
    case EConst(_, v) => v.cost
    case ENode(_, args) => args.map(exprCost).sum
  }
}
