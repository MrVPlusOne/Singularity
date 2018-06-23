package singularity

object CostModel {
  /** Sums up the total cost of an expression. It is used to penalize individuals with
    * complex constant expression during GP. */
  def exprCost(expr: Expr): Double = expr match {
    case _: EArg => 0
    case EConst(_, v) => v.cost
    case ENode(_, args) => args.map(exprCost).sum
  }
}
