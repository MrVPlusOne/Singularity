package singularity

import singularity.GeneticOperator._

import scala.util.Random

/** Used to create new individuals during GP fuzzing processes. */
trait GeneticOperator[Individual]{
  def arity: Int

  def operate(random: Random, participates: IS[Individual]): Individual

  def name: String
}

object GeneticOperator{

  def cartesianProduct[A](listOfSets: IndexedSeq[Iterable[A]]): Iterator[IndexedSeq[A]] = {
    if(listOfSets.isEmpty) return Iterator(IndexedSeq())
    listOfSets.head.toIterator.flatMap(v => cartesianProduct(listOfSets.tail).map(v +: _))
  }

  case class ExprGen[+E <:Expr](returnType: EType, gen: Random => E)

  def genExpr(ty: EType, maxDepth: Int,
              terminalMap: Map[EType, ExprGen[ETerminal]],
              functionMap: Map[EType, IS[EConcreteFunc]],
              random: Random, terminalChance: Double = 0.5
             ): Expr = {
    def randomPick[A](xs: IS[A]): A = {
      val i = random.nextInt(xs.length)
      xs(i)
    }

    def rec(ty: EType, maxDepth: Int): Expr = {
      if(maxDepth <= 0 || random.nextDouble()<terminalChance || !functionMap.contains(ty))
        return terminalMap(ty).gen(random)

      val f = randomPick(functionMap(ty))
      val args = f.argTypes.map{aTy =>
        rec(aTy, maxDepth - 1)
      }
      f(args: _*)
    }

    rec(ty, maxDepth)
  }


  def simpleCrossOver(random: Random, parent: Expr, child: Expr): Expr = {
    val parentSubsTypeMapWithCoord = Expr.subExprs(parent).groupBy(_._2.returnType)
    val childSub2 = Expr.subExprs(child).values
    val childSubsTypeMap = childSub2.groupBy(_.returnType)
    val commonTypes = parentSubsTypeMapWithCoord.keySet intersect childSubsTypeMap.keySet

    val validExprs = childSub2.filter{ expr => commonTypes.contains(expr.returnType)}.toIndexedSeq
    val newChild = validExprs(random.nextInt(validExprs.size))

    val possiblePoints = parentSubsTypeMapWithCoord(newChild.returnType).toIndexedSeq
    val (parentPoint, _) = possiblePoints(random.nextInt(possiblePoints.size))
    Expr.replaceSubExpr(parent, newChild, parentPoint)
  }

  def foldConsts(expr: Expr): EConst = {
    val value = Expr.evaluateWithCheck(expr, IS())
    EConst(expr.returnType, value)
  }

  /** constant folding on all seed expressions */
  def foldConstsMultiInd(ind: MultiStateInd): MultiStateInd = {
    val newSeeds = ind.seeds.map(foldConsts)
    val newExprs = newSeeds ++ ind.exprs.drop(newSeeds.length)
    MultiStateInd(newExprs, ind.nStates)
  }
}

/** Defines the DSL (i.e. which components to use) used to represent input patterns. */
case class GPEnvironment(constMap: Map[EType, ExprGen[EConst]], functions: IS[EFunction], stateTypes: IS[EType], argConstRatio: Double = 0.35, warningOn: Boolean = true) {

  /** type set used to concretize abstract functions, all function argument types and
    * return types must be in this typeUniverse */
  val typeUniverse: Set[EType] = constMap.keySet

  private def signatureTypesInUniverse(sourceName: String, types: Set[EType]): Boolean = {
    types.foreach{t =>
      if(!typeUniverse.contains(t)){
        System.err.println(s"[Warning] $sourceName will not be used, because type $t is not within the type universe.")
        return false
      }
    }
    true
  }

  val functionMap: Map[EType, IndexedSeq[EConcreteFunc]] = functions.flatMap{
    case cf: EConcreteFunc =>
      if(signatureTypesInUniverse(cf.name, cf.argTypes.toSet+cf.returnType)) IS(cf)
      else IS()
    case af: EAbstractFunc =>
      val instantiations = for(
        ts <- cartesianProduct(IS.fill(af.tyVarNum)(typeUniverse)).toIndexedSeq;
        cf = af.concretize(ts) if (cf.argTypes.toSet+cf.returnType).subsetOf(typeUniverse)
      ) yield {
        cf
      }
      if(instantiations.isEmpty && warningOn){
        System.err.println(s"[Warning] ${af.name} will not be used, because there is no valid concrete instantiations under the current type universe")
      }
      instantiations
  }.groupBy(_.returnType)

  require(stateTypes.toSet.subsetOf(typeUniverse), "Some state type is not within the type universe!")
  val args: IndexedSeq[ExprGen[EArg]] = stateTypes.zipWithIndex.map{
    case (t, i) => ExprGen(t, _ =>EArg(i, t))
  }

  val terminalMap: Map[EType, ExprGen[ETerminal]] = {
    stateTypes.zipWithIndex.foldLeft(constMap: Map[EType, ExprGen[ETerminal]]){(map, argPair) => argPair match {
      case (ty, id) =>
      map.updated(ty, ExprGen(ty, r => {
        if(r.nextDouble()<=argConstRatio){
          EArg(id, ty)
        }else{
          map(ty).gen(r)
        }
      }))
    }}
  }

  def show: String = {
    s"""
       |stateTypes = $stateTypes
       |argConstRatio = $argConstRatio
       |constMap: $constMap
       |functions: $functions
     """.stripMargin
  }
}

@deprecated("using multi-state representation instead")
case class SingleStateGOpLibrary(environment: GPEnvironment)  {
  type Individual = SingleStateInd
  type GOp = GeneticOperator[Individual]

  import environment._

  def createIndividual(seed: IS[Expr], iter: IS[Expr]): Individual = {
    SingleStateInd(seed, iter)
  }

  def initOp(maxDepth: Int): GOp = new GOp {
    def name = s"Init"

    override def arity: Int = 0

    override def operate(random: Random, participates: IS[Individual]): Individual = {
      val seed = stateTypes.map(t => genExpr(t, maxDepth, constMap, functionMap, random))
      val iter = stateTypes.map(t => genExpr(t, maxDepth, terminalMap, functionMap, random))
      createIndividual(seed, iter)
    }
  }

  def copyOp: GOp = new GOp {
    def name = "Copy"

    override def arity: Int = 1

    override def operate(random: Random, participates: IS[Individual]): Individual = {
      val IS(ind) = participates
      ind
    }
  }

  def simpleCrossOp(crossSeedProb: Double): GOp = new GOp {
    require(crossSeedProb<=1.0 & crossSeedProb>=0.0)

    def name = "Crossover"

    override def arity: Int = 2

    override def operate(random: Random, participates: IS[Individual]): Individual = {
      val IndexedSeq(parent, child) = participates

      val crossIdx = random.nextInt(parent.seed.length)

      val (newSeed, newExpr) = if(random.nextDouble()<crossSeedProb){
        val e = simpleCrossOver(random, parent.seed(crossIdx), child.seed(crossIdx))
        (parent.seed.updated(crossIdx, e), parent.iter)
      }else{
        val e = simpleCrossOver(random, parent.iter(crossIdx), child.iter(crossIdx))
        (parent.seed, parent.iter.updated(crossIdx,e))
      }
      createIndividual(newSeed, newExpr)
    }
  }

  def simpleMutateOp(newTreeMaxDepth: Int, mutateSeedProb: Double): GOp = new GOp {
    require(mutateSeedProb<=1.0 & mutateSeedProb>=0.0)

    def name = "Mutate"

    override def arity: Int = 1

    override def operate(random: Random, participates: IS[Individual]): Individual = {
      val IndexedSeq(parent) = participates

      val crossIdx = random.nextInt(parent.seed.length)

      val mutateSeed = random.nextDouble() < mutateSeedProb

      val toMutate = if(mutateSeed) parent.seed else parent.iter
      val subs = Expr.subExprs(toMutate(crossIdx)).toIndexedSeq
      val mutationPoint = subs(random.nextInt(subs.length))
      val ty = mutationPoint._2.returnType
      val subExpr = genExpr(ty, newTreeMaxDepth, if(mutateSeed) constMap else terminalMap, functionMap, random)

      val newExpr = Expr.replaceSubExpr(toMutate(crossIdx), subExpr, mutationPoint._1)
      if(mutateSeed)
        createIndividual(parent.seed.updated(crossIdx,newExpr), parent.iter)
      else
        createIndividual(parent.seed, parent.iter.updated(crossIdx,newExpr))
    }
  }
}

/** Provides genetic operators used to create new individuals */
case class MultiStateGOpLibrary(environment: GPEnvironment, outputTypes: IS[EType])  {
  type Individual = MultiStateInd
  type GOp = GeneticOperator[Individual]

  import environment._


  def initOp(maxDepth: Int): GOp = new GOp {
    def name = "Init"

    override def arity: Int = 0

    override def operate(random: Random, participates: IS[Individual]): Individual = {
      val seeds = stateTypes.map(t => genExpr(t, maxDepth, constMap, functionMap, random))
      val iters = stateTypes.map(t => genExpr(t, maxDepth, terminalMap, functionMap, random))
      val outputs = outputTypes.map(t => genExpr(t, maxDepth, terminalMap, functionMap, random))
      MultiStateInd(seeds ++ iters ++ outputs, stateTypes.length)
    }
  }

  def copyOp: GOp = new GOp {
    def name = "Copy"

    override def arity: Int = 1

    override def operate(random: Random, participates: IS[Individual]): Individual = {
      participates.head
    }
  }

  def simpleCrossOp: GOp = new GOp {

    def name = "Crossover"

    override def arity: Int = 2

    override def operate(random: Random, participates: IS[Individual]): Individual = {
      val IndexedSeq(p1,p2) = participates
      val (subs1, subs2) = (p1.allSubExprs, p2.allSubExprs)
      val allSubs = subs1 ++ subs2
      val crossPoint = random.nextInt(allSubs.length)

      val (c, e1) = allSubs(crossPoint)
      val parentSelected = if(crossPoint < subs1.length) p1 else p2
      val seedSelected = parentSelected.isSeed(c._1)

      val t = e1.returnType
      val candidates = allSubs.collect{
        case (_, e) if e.returnType == t && (!seedSelected || e.isConst) => e
      }
      val newSubTree = SimpleMath.randomSelect(random)(candidates)
      parentSelected.update(c, newSubTree)
    }
  }

  def simpleMutateOp(newTreeMaxDepth: Int): GOp = new GOp {

    def name = "Mutate"

    def arity: Int = 1

    def operate(random: Random, participates: IS[Individual]): Individual = {
      val IndexedSeq(parent) = participates

      val allSubs = parent.allSubExprs

      val (c, e) = SimpleMath.randomSelect(random)(allSubs)

      val mapToUse = if(parent.isSeed(c._1)) constMap else terminalMap

      val newSubTree = genExpr(e.returnType, newTreeMaxDepth, mapToUse, functionMap, random)
      parent.update(c, newSubTree)
    }
  }

  /** constant folding on all seed expressions */
  def constantFolding: GOp = new GOp {
    def name = "ConstFold"

    def arity: Int = 1

    def operate(random: Random, participates: IS[Individual]): Individual = {
      foldConstsMultiInd(participates.head)
    }
  }
}
