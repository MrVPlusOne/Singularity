package singularity

/** DSL expressions used to represent input patterns
  * @see [[singularity.MultiStateInd]] */
sealed trait Expr {
  def returnType: EType

  def argTypes: IS[EType]

  override def toString: String = Expr.linearShow(this)

  def astSize: Int

  def isConst: Boolean
}

sealed trait ETerminal extends Expr with SExpr {
  def argTypes: IS[EType] = IS()
}

/** Represents an internal state, used to construct updaters and transformers */
@SerialVersionUID(0L)
case class EArg(id: Int, returnType: EType) extends ETerminal{
  def isConst: Boolean = false

  def astSize: Int = 1
}

@SerialVersionUID(0L)
case class EConst(returnType: EType, value: EValue) extends ETerminal{
  require(value hasType returnType)

  def isConst: Boolean = true

  def astSize: Int = value.memoryUsage.toInt
}

/** Represents applying an function to its argument expressions */
@SerialVersionUID(0L)
case class ENode(f: EConcreteFunc, args: IS[Expr]) extends Expr {
  val astSize: Int = args.map(_.astSize).sum + 1

  val isConst: Boolean = args.forall(_.isConst)

  f.argTypes.zip(args).foreach{
    case (t, expr) =>
      if(t != expr.returnType){
        throw new Exception(
          s"""
             |Argument type not match in $toString
             |argument $expr has type ${expr.returnType}, but it's expected to have type $t
           """.stripMargin)
        throw new Exception(s"Argument $expr has type ${expr.returnType}, " +
          s"but it's expected to have type $t.")
      }
  }

  override def returnType: EType = f.returnType

  override def argTypes: IS[EType] = f.argTypes
}

sealed trait EFunction{
  def name: String
}

/** An DSL function. Corresponds to a "Component" described in the Singularity paper.
  * @see [[singularity.StandardSystem]], [[singularity.StandardSystem.IntComponents]], etc.. */
@SerialVersionUID(0L)
case class EConcreteFunc(name: String, argTypes: IS[EType], returnType: EType,
                         eval: PartialFunction[IS[EValue], EValue]) extends EFunction {
  def apply (args: Expr*): ENode = {
    ENode(this, args.toIndexedSeq)
  }

  override def toString: String = {
    s"$name: ${argTypes.mkString("(",",",")")} => $returnType"
  }
}

/** An abstract DSL function. Can be turned into a concrete function when applied to type parameters.
  * @see [[singularity.EConcreteFunc]], [[singularity.StandardSystem]]*/
@SerialVersionUID(0L)
case class EAbstractFunc(name: String, tyVarNum: Int,
                         typeInstantiation: (IS[EType]) => (IS[EType], EType),
                         eval: PartialFunction[IS[EValue], EValue]) extends EFunction {

  def concretize(tyMap: IS[EType]): EConcreteFunc = {
    val (argTypes, returnType) = typeInstantiation(tyMap)
    EConcreteFunc(name, argTypes, returnType, eval)
  }

  override def toString: String = {
    s"$name[$tyVarNum]"
  }
}


object Expr {

  def evaluateWithCheck(expr: Expr, env: IndexedSeq[EValue]): EValue = {
//    println(s"$expr \\||/ $env")
    def withType(ty: EType)(value: EValue) ={
      assert(value.hasType(ty), s"$value should have type $ty")
      value
    }
    expr match {
      case arg: EArg => withType(expr.returnType){ env(arg.id) }
      case EConst(_, value) => value
      case ENode(f, args) => withType(f.returnType){
        f.eval(args.map(a => evaluateWithCheck(a, env)))
      }
    }
  }

  def iterationStream(seed: EValue, expr: Expr): Stream[EValue] = {
    def f(v: EValue): EValue = {
      evaluateWithCheck(expr, IS(v))
    }
    Stream.iterate(seed)(f)
  }

  def linearShowWithType(expr: Expr): String = expr match {
    case EArg(id, ty) => s"#$id: $ty"
    case EConst(t, value) => s"$value: ${t}"
    case ENode(f, args) => s"${f.name}(${args.map(linearShowWithType).mkString(", ")}): ${f.returnType}"
  }

  def linearShow(expr: Expr): String = expr match {
    case EArg(id, _) => s"#$id"
    case EConst(_, value) => s"$value"
    case ENode(f, args) => s"${f.name}(${args.map(linearShow).mkString(", ")})"
  }

  type Coordinate = IS[Int]

  def subExprs(expr: Expr): Map[Coordinate,Expr] = {
    def rec(baseCoord: Coordinate, expr: Expr): Map[Coordinate,Expr] = expr match {
      case term: ETerminal => Map(baseCoord->term)
      case n@ENode(f, args) => args.zipWithIndex.foldRight(Map(baseCoord->(n: Expr))){ (argIdx, acc) =>
        val (arg, idx) = argIdx
        rec(baseCoord:+idx, arg) ++ acc
      }
    }
    rec(IS(), expr)
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

}

/** S Expressions used for serialization purpose. (use a function map during saving/restoration
  * to get rid of lambdas) */
sealed trait SExpr

/** used for serialization purpose. (use a function map during saving/restoration
  * to get rid of lambdas) */
case class SNode(name: String, argTypes: IS[EType], returnType: EType,
                 args: IS[SExpr]
                ) extends SExpr

object SExpr {
  def mkSExpr(expr: Expr): SExpr = expr match {
    case terminal: ETerminal => terminal
    case ENode(f, args) =>
      SNode(f.name, f.argTypes, f.returnType, args.map(mkSExpr))
  }

  def mkExpr(sExpr: SExpr, funcMap: Map[String, EFunction]): Expr = {
    sExpr match {
      case SNode(name, argTypes, returnType, args) =>
        val f = funcMap.getOrElse(name, throw new Exception(s"function $name not found."))
        val eArgs = args.map(a => mkExpr(a, funcMap))
        f match {
          case cf: EConcreteFunc => ENode(cf, eArgs)
          case af: EAbstractFunc =>
            val cf = EConcreteFunc(name, argTypes, returnType, af.eval)
            ENode(cf, eArgs)
        }
      case t: ETerminal => t
    }
  }
}
