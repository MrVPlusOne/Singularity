package patsyn


sealed trait Expr {
  def returnType: EType

  def argTypes: IS[EType]

  override def toString: String = Expr.linearShow(this)

  def astSize: Int
}

sealed trait ETerminal extends Expr {
  def argTypes: IS[EType] = IS()

  override def astSize: Int = 1
}

case class EArg(id: Int, returnType: EType) extends ETerminal{
}

case class EConst(returnType: EType, value: EValue) extends ETerminal{
  require(value hasType returnType)
}

case class ENode(f: EConcreteFunc, args: IS[Expr]) extends Expr {
  val astSize: Int = args.map(_.astSize).sum + 1

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

sealed trait EFunction

case class EConcreteFunc(name: String, argTypes: IS[EType], returnType: EType,
                         eval: PartialFunction[IS[EValue], EValue]) extends EFunction {
  def apply (args: Expr*): ENode = {
    ENode(this, args.toIndexedSeq)
  }

  override def toString: String = {
    s"$name: ${argTypes.mkString("(",",",")")} => $returnType"
  }
}

case class EAbstractFunc(name: String, tyVarNum: Int,
                         typeInstantiation: (IS[EType]) => (IS[EType], EType),
                         eval: PartialFunction[IS[EValue], EValue]) extends EFunction {

  def concretize(tyMap: IS[EType]): EConcreteFunc = {
    val (argTypes, returnType) = typeInstantiation(tyMap)
    EConcreteFunc(name, argTypes, returnType, eval)
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