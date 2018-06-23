package singularity

import singularity.EvolutionRepresentation.MemoryUsage
import singularity.MultiStateInd.GlobalCoord

object MultiStateInd {
  type GlobalCoord = (Int, Expr.Coordinate)
}

/** An individual consists of multiple internal states and iterators. Corresponds to "Recurrent Computation Graph"
  * in the Singularity paper.
  * @param exprs all expressions in this individual. equals to ``seeds ++ iters ++ outputs``
  * @param nStates number of internal states
  * */
@SerialVersionUID(0L)
case class MultiStateInd(exprs: IS[Expr], nStates: Int){
  val seeds: IS[Expr] = exprs.take(nStates)
  val iters: IS[Expr] = (nStates until 2*nStates).map(i => exprs(i))
  val outputs: IS[Expr] = exprs.drop(2*nStates)

  def totalAstSize: Int = {
    exprs.map(_.astSize).sum
  }

  lazy val allSubExprs: IS[(GlobalCoord, Expr)] = {
    exprs.indices.flatMap(i =>
      Expr.subExprs(exprs(i)).map{
        case (c, e) => (i, c) -> e
      }
    )
  }

  def isSeed(id: Int): Boolean = id < nStates

  def update(coord: GlobalCoord, newSubTree: Expr): MultiStateInd = {
    val (i, eCoord) = coord
    val newExpr = Expr.replaceSubExpr(exprs(i), newSubTree, eCoord)
    this.copy(exprs.updated(i, newExpr), nStates)
  }
}

object MultiStateRepresentation{

  def printIndividualMultiLine(println: String => Unit, stateTypes: IS[EType],
                               outputTypes: IS[EType], ind: MultiStateInd): Unit = {
    ind.outputs.indices.foreach { i =>
      val t = outputTypes(i)
      val out = ind.outputs(i)
      println(s"[O$i: $t] -> $out")
    }
    println("*")
    stateTypes.indices.foreach { i =>
      val t = stateTypes(i)
      val s = ind.seeds(i)
      val iter = ind.iters(i)
      val seedValue = Expr.evaluateWithCheck(s, IS())
      println(s"[S$i: $t]{ seed: $seedValue ; iter: $iter; seedExpr: $s }")
    }
  }

  def individualToPattern(ind: MultiStateInd): Stream[(MemoryUsage, IS[EValue])] = {
    val iters = ind.iters
    val initStates = ind.seeds.map(seed => Expr.evaluateWithCheck(seed, IS()))
    val states = Stream.iterate(initStates)(ls => {
      iters.map { iter => Expr.evaluateWithCheck(iter, ls) }
    })
    states.map(xs => {
      val totalSize = xs.map(_.memoryUsage).sum
      MemoryUsage(totalSize) -> ind.outputs.map(f => Expr.evaluateWithCheck(f, xs))
    })
  }

  def saveExtrapolation(problemConfig: ProblemConfig, individual: MultiStateInd,
                        sizeLimit: Int, memoryLimit: Long, name: String, evaluatePerformance: Boolean = false): Unit = {
    import EvolutionRepresentation.MemoryUsage

    println(s"Calculating extrapolation at size = $sizeLimit ...")
    var lastSize = Int.MinValue
    var progress = 0

    val valueOfInterest = individualToPattern(individual).takeWhile {
      case (MemoryUsage(memory), value) =>
        val newSize = problemConfig.sizeF(value)
        if (newSize <= lastSize) {
          println("Warning: Can't reach specified size using this individual")
          false
        } else {
          progress += 1

          lastSize = newSize
          newSize <= sizeLimit && memory <= memoryLimit
        }
    }.last._2

    println(s"Extrapolation calculated. Now save results to $name")
    problemConfig.saveValueWithName(valueOfInterest, name)

    if (evaluatePerformance) {
      println("Performance evaluation started...")
      val performance = problemConfig.resourceUsage(valueOfInterest)
      println(s"Performance: $performance")
    }
  }
}

/** Provides utility operations needed for applying Genetic Programming to [[singularity.MultiStateInd]] */
case class MultiStateRepresentation(stateTypes: IS[EType], outputTypes: IS[EType],
                                    totalSizeTolerance: Double, singleSizeTolerance: Double,
                                    exprCostPenaltyBase: Double,
                                    evaluation: PerformanceEvaluation)
  extends EvolutionRepresentation[MultiStateInd] {

  def showIndividual(ind: MultiStateInd): String = {
    (ind.outputs.indices.map { i =>
      val t = outputTypes(i)
      val out = ind.outputs(i)
      s"[O$i: $t]{ $out }"
    } ++ stateTypes.indices.map { i =>
      val t = stateTypes(i)
      val s = ind.seeds(i)
      val iter = ind.iters(i)
      s"[S$i: $t]{ seed: $s ; iter: $iter }"
    }).mkString("< ", " | ", " >")
  }

  def costPenalty(ind: MultiStateInd): Double = {
    val totalCost = ind.exprs.map(CostModel.exprCost).sum
    math.pow(exprCostPenaltyBase, totalCost)
  }

  def showIndividualMultiLine(ind: MultiStateInd): String = {
    val s = new StringBuilder()
    printIndividualMultiLine(x => {
      s.append(x)
      s.append("\n")
    })(ind)
    s.mkString
  }

  def printIndividualMultiLine(println: String => Unit)(ind: MultiStateInd): Unit = {
    MultiStateRepresentation.printIndividualMultiLine(println, stateTypes, outputTypes, ind)
  }

  def representationSize(ind: MultiStateInd): Double = {
    ind.totalAstSize.toDouble
  }

  def individualExprs(ind: MultiStateInd): Seq[Expr] = {
    ind.allSubExprs.map(_._2)
  }

  def individualToPattern(ind: MultiStateInd): Stream[(MemoryUsage, IS[EValue])] = {
    MultiStateRepresentation.individualToPattern(ind)
  }

  def sizePenaltyFactor(ind: MultiStateInd): Double = {
    import SimpleMath.gaussianForthOrder
    ind.exprs.map(e => gaussianForthOrder(singleSizeTolerance)(e.astSize)).product *
      gaussianForthOrder(totalSizeTolerance)(ind.totalAstSize)
  }

  def isTooLarge(ind: MultiStateInd): Boolean = ind.totalAstSize > totalSizeTolerance
}
