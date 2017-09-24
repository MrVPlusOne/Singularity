package patsyn

import Evolution.{Individual}
import GeneticOpLibrary._

import scala.util.Random

trait GeneticOperator{
  def arity: Int

  def operate(random: Random, participates: IS[Individual]): Individual
}

object GeneticOpLibrary{

  def cartesianProduct[A](listOfSets: IndexedSeq[Iterable[A]]): Iterator[IndexedSeq[A]] = {
    if(listOfSets.isEmpty) return Iterator(IndexedSeq())
    listOfSets.head.toIterator.flatMap(v => cartesianProduct(listOfSets.tail).map(v +: _))
  }

  case class ExprGen[+E <:Expr](returnType: EType, gen: Random => E)

  def genExpr(ty: EType, maxDepth: Int,
              terminalMap: Map[EType, IS[ExprGen[ETerminal]]],
              functionMap: Map[EType, IS[EConcreteFunc]],
              random: Random, terminalChance: Double = 0.5
             ): Expr = {
    def randomPick[A](xs: IS[A]): A = {
      val i = random.nextInt(xs.length)
      xs(i)
    }

    def rec(ty: EType, maxDepth: Int): Expr = {
      if(maxDepth <= 0 || random.nextDouble()<terminalChance || !functionMap.contains(ty))
        return randomPick(terminalMap(ty)).gen(random)

      val f = randomPick(functionMap(ty))
      val args = f.argTypes.map{aTy =>
        rec(aTy, maxDepth - 1)
      }
      f(args: _*)
    }

    rec(ty, maxDepth)
  }

  def replaceSubExpr(parent: Expr, child: Expr, replacePoint: Expr.Coordinate): Expr = {
    def rec(parent: Expr, point: Expr.Coordinate): Expr = {
      if(point.isEmpty) child
      else parent match {
        case ENode(f, args) =>
          val newArgs = args.updated(point.head, rec(args(point.head), point.tail))
          ENode(f, newArgs)
        case _ => throw new Exception("Invalid coordinate")
      }
    }
    rec(parent, replacePoint)
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
    replaceSubExpr(parent, newChild, parentPoint)
  }
}

class GeneticOpLibrary(constMap: Map[EType, IS[ExprGen[EConst]]], functions: IS[EFunction],
                       seedTypes: IS[EType]
                      ){

  require(constMap.forall{case (_, gens) => gens.nonEmpty}, "For each type in constMap, there should be at least one ExprGen.")
  /** type set used to concretize abstract functions */
  val typeUniverse: Set[EType] = constMap.keySet

  private def argTypesInTheUniverse(sourceName: String, types: Set[EType]): Boolean = {
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
      if(argTypesInTheUniverse(cf.name, cf.argTypes.toSet)) IS(cf)
      else IS()
    case af: EAbstractFunc =>
      val instantiations = for(
        ts <- cartesianProduct(IS.fill(af.tyVarNum)(typeUniverse)).toIndexedSeq;
        cf = af.concretize(ts) if cf.argTypes.toSet.subsetOf(typeUniverse)
      ) yield {
        cf
      }
      if(instantiations.isEmpty){
        System.err.println(s"[Warning] ${af.name} will not be used, because there is no valid concrete instantiations under the current type universe")
      }
      instantiations
  }.groupBy(_.returnType)

  require(seedTypes.toSet.subsetOf(typeUniverse), "Some seed type is not within the type universe!")
  val args: IndexedSeq[ExprGen[EArg]] = seedTypes.zipWithIndex.map{
    case (t, i) => ExprGen(t, _ =>EArg(i, t))
  }

  val terminalMap: Map[EType, IS[ExprGen[ETerminal]]] = {
    args.foldLeft(constMap: Map[EType, IS[ExprGen[ETerminal]]]){(map, gen) => {
      map.updated(gen.returnType, gen +: map(gen.returnType))
    }}
  }

  def createIndividual(seed: IS[Expr], iter: IS[Expr]): Individual = {
    Individual(seed, iter)
  }

  def initOp(maxDepth: Int): GeneticOperator = new GeneticOperator {
    override def arity: Int = 0

    override def operate(random: Random, participates: IS[Individual]): Individual = {
      val seed = seedTypes.map(t => genExpr(t, maxDepth, constMap, functionMap, random))
      val iter = seedTypes.map(t => genExpr(t, maxDepth, terminalMap, functionMap, random))
      createIndividual(seed, iter)
    }
  }

  def copyOp: GeneticOperator = new GeneticOperator {
    override def arity: Int = 1

    override def operate(random: Random, participates: IS[Individual]): Individual = {
      val IS(ind) = participates
      ind
    }
  }

  def simpleCrossOp(crossSeedProb: Double): GeneticOperator = new GeneticOperator {
    require(crossSeedProb<=1.0 & crossSeedProb>=0.0)

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

  def simpleMutateOp(newTreeMaxDepth: Int, mutateSeedProb: Double): GeneticOperator = new GeneticOperator {
    require(mutateSeedProb<=1.0 & mutateSeedProb>=0.0)

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

      val newExpr = replaceSubExpr(toMutate(crossIdx), subExpr, mutationPoint._1)
      if(mutateSeed)
        createIndividual(parent.seed.updated(crossIdx,newExpr), parent.iter)
      else
        createIndividual(parent.seed, parent.iter.updated(crossIdx,newExpr))
    }
  }
}
