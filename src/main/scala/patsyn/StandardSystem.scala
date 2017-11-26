package patsyn

import scala.language.implicitConversions

//noinspection TypeAnnotation
object StandardSystem {

  // Type declarations
  case object EInt extends EType

  case class EVect(elemT: EType) extends EType

  case object EBool extends EType

  case class EPair(t1: EType, t2: EType) extends EType

  case class EGraph(edgeT: EType) extends EType

  // Value declarations
  case class IntValue(value: Int) extends EValue {
    def hasType(ty: EType): Boolean = ty == EInt

    override def toString: String = value.toString

    def size: Long = 1
  }

  case class VectValue(value: Vector[EValue]) extends EValue{
    def hasType(ty: EType): Boolean = ty match {
      case EVect(et) => value.isEmpty || value.head.hasType(et)
      case _ => false
    }

    override def toString: String = value.mkString("[",",","]")

    def size: Long = value.map(_.size).sum + 1
  }

  case class BoolValue(value: Boolean) extends EValue{
    def hasType(ty: EType): Boolean = ty == EBool

    def size: Long = 1

    override def toString: String = if(value)"T" else "F"
  }

  case class PairValue(value: (EValue, EValue)) extends EValue{
    def hasType(ty: EType): Boolean = ty match {
      case EPair(t1, t2) => value._1.hasType(t1) && value._2.hasType(t2)
      case _ => false
    }

    def size: Long = value._1.size + value._2.size

    override def toString: String = value.toString()
  }

  case class GraphValue(nodeNum: Int, edges: IS[(Int, Int, EValue)]) extends EValue{
    def hasType(ty: EType): Boolean = ty match {
      case EGraph(edgeT) => edges.isEmpty || edges.head._3.hasType(edgeT)
      case _ => false
    }

    def size: Long = nodeNum + edges.map(_._3.size).sum + 1

    def shiftIndex(offset: Int): GraphValue = {
      this.copy(edges = edges.map{ case (n1, n2, v) => (n1+offset, n2+offset, v)})
    }
  }

  implicit def intValue(v: Int): IntValue = IntValue(v)

  implicit def vectValue[E](v: Vector[EValue]): VectValue = {
    VectValue(v)
  }

  implicit def boolValue(b: Boolean): BoolValue = BoolValue(b)

  implicit def pairValue[A,B](p: (A, B))(implicit convA: A => EValue, convB: B => EValue): PairValue = {
    PairValue(convA(p._1) -> convB(p._2))
  }

  trait ComponentSet{
    private var _collection = IS[EFunction]()

    def collection: IS[EFunction] = _collection

    def mkConcrete(name: String, argTypes: IS[EType], returnType: EType,
                   eval: PartialFunction[IS[EValue], EValue]): EConcreteFunc = {
      val f = EConcreteFunc(name, argTypes, returnType, eval)
      _collection :+= f
      f
    }

    def mkAbstract(name: String, tyVarNum: Int,
                   typeInstantiation: (IS[EType]) => (IS[EType], EType),
                   eval: PartialFunction[IS[EValue], EValue]): EAbstractFunc = {
      val f = EAbstractFunc(name, tyVarNum, typeInstantiation, eval)
      _collection :+= f
      f
    }
  }

  object IntComponents extends ComponentSet {
    val inc =  mkConcrete("inc", IS(EInt), EInt, {
      case IS(IntValue(i)) => i + 1
    })

    val dec =  mkConcrete("dec", IS(EInt), EInt, {
      case IS(IntValue(i)) => i - 1
    })

    val neg =  mkConcrete("neg", IS(EInt), EInt, {
      case IS(IntValue(i)) => -i
    })

    val plus =  mkConcrete("plus", IS(EInt, EInt), EInt, {
      case IS(IntValue(a), IntValue(b)) => a + b
    })

    val minus =  mkConcrete("minus", IS(EInt, EInt), EInt, {
      case IS(IntValue(a), IntValue(b)) => a - b
    })

    val times =  mkConcrete("times", IS(EInt, EInt), EInt, {
      case IS(IntValue(a), IntValue(b)) => a * b
    })

    /** protected divide */
    val divide =  mkConcrete("divide", IS(EInt, EInt), EInt, {
      case IS(IntValue(a), IntValue(b)) => if(b==0) 1 else a / b
    })

    val modular = mkConcrete("modular", IS(EInt, EInt), EInt, {
      case IS(IntValue(a), IntValue(b)) => if(b==0) 1 else a % b
    })

  }

  object BitComponents extends ComponentSet {
    val shiftByteLeft = mkConcrete("shiftBL", IS(EInt), EInt, {
      case IS(IntValue(a)) => a << 8
    })

    val bitAnd = mkConcrete("bitAnd", IS(EInt, EInt), EInt, {
      case IS(IntValue(a), IntValue(b)) => a & b
    })
    val bitOr = mkConcrete("bitOr", IS(EInt, EInt), EInt, {
      case IS(IntValue(a), IntValue(b)) => a | b
    })
    val bitXor = mkConcrete("bitXor", IS(EInt, EInt), EInt, {
      case IS(IntValue(a), IntValue(b)) => a ^ b
    })
    val bitShift = mkConcrete("bitShift", IS(EInt, EInt), EInt, {
      case IS(IntValue(a), IntValue(b)) =>
        if (b >= 0)
          a << b
        else
          a >>> (-b)
    })
  }

  object VectComponents extends ComponentSet {
    val append = mkAbstract("append", 1, {
      case IS(e) => IS(EVect(e), e) -> EVect(e)
    }, {
      case IS(VectValue(vec), v) => vec :+ v
    })

    val prepend = mkAbstract("prepend", 1, {
      case IS(e) => IS(e, EVect(e)) -> EVect(e)
    }, {
      case IS(v, VectValue(vec)) => v +: vec
    })

    val access = mkAbstract("access", 1, {
      case IS(e) => IS(EVect(e), EInt, e) -> e
    }, {
      case IS(VectValue(vec), IntValue(v), default) =>
        if (vec.isEmpty) default
        else {
          val i = SimpleMath.wrapInRange(v, vec.length)
          vec(i)
        }
    })

    val concat = mkAbstract("concat", 1, {
      case IS(e) => IS(EVect(e), EVect(e)) -> EVect(e)
    }, {
      case IS(VectValue(v1), VectValue(v2)) => v1 ++ v2
    })

    val length = mkAbstract("length", 1, {
      case IS(e) => IS(EVect(e)) -> EInt
    }, {
      case IS(VectValue(v1)) => v1.length
    })
  }

  object AdvancedVectComponents extends ComponentSet{
    val shift = mkConcrete("shift", IS(EVect(EInt), EInt), EVect(EInt), {
      case IS(VectValue(vec), IntValue(v)) => vec.map{
        case IntValue(i) => IntValue(i+v)
      }
    })

    val intToString = mkConcrete("int2String", IS(EInt, EInt), EVect(EInt), {
      case IS(IntValue(x), IntValue(base)) =>
        val p = if(x<0) -x else x
        val b = if(base < 10) 10 else base
        SimpleMath.natToList(p, b).toVector.map(IntValue.apply)
    })
  }

  object BoolComponents extends ComponentSet {
    val not = mkConcrete("not", IS(EBool), EBool, {
      case IS(BoolValue(b)) => !b
    })

    val and = mkConcrete("and", IS(EBool, EBool), EBool, {
      case IS(BoolValue(b1), BoolValue(b2)) => b1 && b2
    })

    val or = mkConcrete("or", IS(EBool, EBool), EBool, {
      case IS(BoolValue(b1), BoolValue(b2)) => b1 || b2
    })

    val lessThan = mkConcrete("lessThan", IS(EInt, EInt), EBool, {
      case IS(IntValue(i1), IntValue(i2)) => i1 < i2
    })

    val equal = mkAbstract("equal", 1, {
      case IS(e) => IS(e, e) -> EBool
    }, {
      case IS(v1, v2) => v1 == v2
    })

    val ifElse = mkAbstract("ifElse", 1, {
      case IS(e) => IS(EBool, e, e) -> e
    }, {
      case IS(BoolValue(b), v1, v2) => if (b) v1 else v2
    })

  }

  object PairComponents extends ComponentSet {
    val pair1 = mkAbstract("pair1", tyVarNum = 2,
      typeInstantiation = {
        case IS(t1,t2) => IS(EPair(t1,t2)) -> t1
      }, eval = {
        case IS(PairValue(value)) => value._1
      }
    )

    val pair2 = mkAbstract("pair2", tyVarNum = 2,
      typeInstantiation = {
        case IS(t1,t2) => IS(EPair(t1,t2)) -> t2
      }, eval = {
        case IS(PairValue(value)) => value._2
      }
    )

    val mkPair = mkAbstract("mkPair", tyVarNum = 2,
      typeInstantiation = {
        case IS(t1,t2) => IS(t1,t2) -> EPair(t1,t2)
      }, eval = {
        case IS(v1,v2) => (v1,v2)
      }
    )
  }

  /** add an isolated vertex */
  object GraphComponents extends ComponentSet {
    val emptyGraph = mkAbstract("emptyGraph", tyVarNum = 1,
      typeInstantiation = {
        case IS(eT) => IS() -> EGraph(eT)
      }, eval = {
        case IS() => GraphValue(0, IS())
      })

    val addNode = mkAbstract("addNode", tyVarNum = 1,
      typeInstantiation = {
        case IS(eT) => IS(EGraph(eT)) -> EGraph(eT)
      }, eval = {
        case IS(gv: GraphValue) => gv.copy(nodeNum = gv.nodeNum+1)
      })

    /** add an isolated edge */
    val addEdge = mkAbstract("addEdge", tyVarNum = 1,
      typeInstantiation = {
        case IS(eT) => IS(EGraph(eT), eT) -> EGraph(eT)
      }, eval = {
        case IS(GraphValue(nodeNum, edges), edgeValue) =>
          val newEdges = edges :+ (nodeNum, nodeNum+1, edgeValue)
          GraphValue(nodeNum + 2, newEdges)
      })

    /** add an edge from/to an existing vertex */
    val growEdge = mkAbstract("growEdge", tyVarNum = 1,
      typeInstantiation = {
        case IS(eT) => IS(EGraph(eT), EInt, eT) -> EGraph(eT)
      }, eval = {
        case IS(g @ GraphValue(nodeNum, edges), IntValue(nodeToGrow), edgeValue) =>
          if(nodeNum == 0) g
          else{
            val n = SimpleMath.wrapInRange(nodeToGrow, nodeNum)
            val newEdges = edges :+ (n, nodeNum, edgeValue)
            GraphValue(nodeNum + 1, newEdges)
          }
      })

    /** add a self loop to a vertex */
    val growSelfLoop = mkAbstract("growSelfLoop", tyVarNum = 1,
      typeInstantiation = {
        case IS(eT) => IS(EGraph(eT), EInt, eT) -> EGraph(eT)
      }, eval = {
        case IS(g @ GraphValue(nodeNum, edges), IntValue(nodeToGrow), edgeValue) =>
          if(nodeNum == 0) g
          else{
            val n = SimpleMath.wrapInRange(nodeToGrow, nodeNum)
            val newEdges = edges :+ (n, n, edgeValue)
            GraphValue(nodeNum, newEdges)
          }
      })

    /** add an edge between two existing vertices */
    val bridgeEdge = mkAbstract("bridgeEdge", tyVarNum = 1,
      typeInstantiation = {
        case IS(eT) => IS(EGraph(eT), EInt, EInt, eT) -> EGraph(eT)
      }, eval = {
        case IS(g @ GraphValue(nodeNum, edges), IntValue(from), IntValue(to), edgeValue) =>
          if(nodeNum == 0) g
          else{
            val n1 = SimpleMath.wrapInRange(from, nodeNum)
            val n2 = SimpleMath.wrapInRange(to, nodeNum)
            val newEdges = edges :+ (n1, n2, edgeValue)
            GraphValue(nodeNum, newEdges)
          }
      })

    /** delete an edge */
    val deleteEdge = mkAbstract("deleteEdge", tyVarNum = 1,
      typeInstantiation = {
        case IS(eT) => IS(EGraph(eT), EInt) -> EGraph(eT)
      }, eval = {
        case IS(g @ GraphValue(nodeNum, edges), IntValue(edgeIndex)) =>
          if(edges.isEmpty) g
          else{
            val e = SimpleMath.wrapInRange(edgeIndex, nodeNum)
            val newEdges = edges.patch(e, IS(), e+1)
            GraphValue(nodeNum, newEdges)
          }
      })

    val mergeGraph = mkAbstract("mergeGraph", tyVarNum = 1,
      typeInstantiation = {
        case IS(eT) => IS(EGraph(eT), EGraph(eT)) -> EGraph(eT)
      }, eval = {
        case IS(g1: GraphValue, g2: GraphValue) =>
          GraphValue(g1.nodeNum + g2.nodeNum, g2.shiftIndex(g1.nodeNum).edges)
      })

    val updateEdgeValue = mkAbstract("updateEdge", tyVarNum = 1,
      typeInstantiation = {
        case IS(eT) => IS(EGraph(eT), EInt, eT) -> EGraph(eT)
      }, eval = {
        case IS(g1: GraphValue, eId: IntValue, v: EValue) =>
          val eNum = g1.edges.length
          if(eNum==0) g1
          else{
            val index = SimpleMath.wrapInRange(eId.value, eNum)
            val (from, to, _) = g1.edges(index)
            val newEdges = g1.edges.updated(index, (from, to, v))
            GraphValue(g1.nodeNum, newEdges)
          }
      }
    )
  }
}
