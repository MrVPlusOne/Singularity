package patsyn

import scala.language.implicitConversions

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
  }

  implicit def intValue(v: Int): IntValue = IntValue(v)

  implicit def vectValue[E](v: Vector[EValue]): VectValue = {
    VectValue(v)
  }

  implicit def boolValue(b: Boolean): BoolValue = BoolValue(b)

  implicit def pairValue[A,B](p: (A, B))(implicit convA: A => EValue, convB: B => EValue): PairValue = {
    PairValue(convA(p._1) -> convB(p._2))
  }

  object IntComponents{
    val inc =  EConcreteFunc("inc", IS(EInt), EInt, {
      case IS(IntValue(i)) => i + 1
    })

    val dec =  EConcreteFunc("dec", IS(EInt), EInt, {
      case IS(IntValue(i)) => i - 1
    })

    val neg =  EConcreteFunc("neg", IS(EInt), EInt, {
      case IS(IntValue(i)) => -i
    })

    val plus =  EConcreteFunc("plus", IS(EInt, EInt), EInt, {
      case IS(IntValue(a), IntValue(b)) => a + b
    })

    val minus =  EConcreteFunc("minus", IS(EInt, EInt), EInt, {
      case IS(IntValue(a), IntValue(b)) => a - b
    })

    val times =  EConcreteFunc("times", IS(EInt, EInt), EInt, {
      case IS(IntValue(a), IntValue(b)) => a * b
    })

    /** protected divide */
    val divide =  EConcreteFunc("divide", IS(EInt, EInt), EInt, {
      case IS(IntValue(a), IntValue(b)) => if(b==0) 1 else a / b
    })

    val modular = EConcreteFunc("modular", IS(EInt, EInt), EInt, {
      case IS(IntValue(a), IntValue(b)) => if(b==0) 1 else a % b
    })

    val collection: IndexedSeq[EFunction] = IS(inc, dec, neg, plus, minus, times, divide, modular)

    val shiftByteLeft = EConcreteFunc("shiftBL", IS(EInt), EInt, {
      case IS(IntValue(a)) => a << 8
    })

    val bitAnd = EConcreteFunc("bitAnd", IS(EInt, EInt), EInt, {
      case IS(IntValue(a), IntValue(b)) => a & b
    })
    val bitOr = EConcreteFunc("bitOr", IS(EInt, EInt), EInt, {
      case IS(IntValue(a), IntValue(b)) => a | b
    })
    val bitXor = EConcreteFunc("bitXor", IS(EInt, EInt), EInt, {
      case IS(IntValue(a), IntValue(b)) => a ^ b
    })
    val bitShift = EConcreteFunc("bitShift", IS(EInt, EInt), EInt, {
      case IS(IntValue(a), IntValue(b)) =>
        if (b >= 0)
          a << b
        else
          a >>> (-b)
    })

    val bitCollection: IndexedSeq[EFunction] = IS(bitAnd, bitOr, bitXor, bitShift)
  }

  object VectComponents {
    val append = EAbstractFunc("append", 1, {
      case IS(e) => IS(EVect(e), e) -> EVect(e)
    }, {
      case IS(VectValue(vec), v) => vec :+ v
    })

    val prepend = EAbstractFunc("prepend", 1, {
      case IS(e) => IS(e, EVect(e)) -> EVect(e)
    }, {
      case IS(v, VectValue(vec)) => v +: vec
    })

    val access = EAbstractFunc("access", 1, {
      case IS(e) => IS(EVect(e), EInt, e) -> e
    }, {
      case IS(VectValue(vec), IntValue(v), default) =>
        if (vec.isEmpty) default
        else {
          val i = SimpleMath.wrapInRange(v, vec.length)
          vec(i)
        }
    })

    val concat = EAbstractFunc("concat", 1, {
      case IS(e) => IS(EVect(e), EVect(e)) -> EVect(e)
    }, {
      case IS(VectValue(v1), VectValue(v2)) => v1 ++ v2
    })

    val length = EAbstractFunc("length", 1, {
      case IS(e) => IS(EVect(e)) -> EInt
    }, {
      case IS(VectValue(v1)) => v1.length
    })

    val shift = EConcreteFunc("shift", IS(EVect(EInt), EInt), EVect(EInt), {
      case IS(VectValue(vec), IntValue(v)) => vec.map{
        case IntValue(i) => IntValue(i+v)
      }
    })

    val intToString = EConcreteFunc("int2String", IS(EInt, EInt), EVect(EInt), {
      case IS(IntValue(x), IntValue(base)) =>
        val p = if(x<0) -x else x
        val b = if(base < 10) 10 else base
        SimpleMath.natToList(p, b).toVector.map(IntValue.apply)
    })

    val collection: IndexedSeq[EFunction] = IS(append, prepend, access, concat, length)
  }

  object BoolComponents {
    val not = EConcreteFunc("not", IS(EBool), EBool, {
      case IS(BoolValue(b)) => !b
    })

    val and = EConcreteFunc("and", IS(EBool, EBool), EBool, {
      case IS(BoolValue(b1), BoolValue(b2)) => b1 && b2
    })

    val or = EConcreteFunc("or", IS(EBool, EBool), EBool, {
      case IS(BoolValue(b1), BoolValue(b2)) => b1 || b2
    })

    val lessThan = EConcreteFunc("lessThan", IS(EInt, EInt), EBool, {
      case IS(IntValue(i1), IntValue(i2)) => i1 < i2
    })

    val equal = EAbstractFunc("equal", 1, {
      case IS(e) => IS(e, e) -> EBool
    }, {
      case IS(v1, v2) => v1 == v2
    })

    val ifElse = EAbstractFunc("ifElse", 1, {
      case IS(e) => IS(EBool, e, e) -> e
    }, {
      case IS(BoolValue(b), v1, v2) => if (b) v1 else v2
    })

    val collection: IndexedSeq[EFunction] = IS(not, and, or, lessThan, equal, ifElse)
  }

  object PairComponents {
    val pair1 = EAbstractFunc("pair1", tyVarNum = 2,
      typeInstantiation = {
        case IS(t1,t2) => IS(EPair(t1,t2)) -> t1
      }, eval = {
        case IS(PairValue(value)) => value._1
      }
    )

    val pair2 = EAbstractFunc("pair2", tyVarNum = 2,
      typeInstantiation = {
        case IS(t1,t2) => IS(EPair(t1,t2)) -> t2
      }, eval = {
        case IS(PairValue(value)) => value._2
      }
    )

    val mkPair = EAbstractFunc("mkPair", tyVarNum = 2,
      typeInstantiation = {
        case IS(t1,t2) => IS(t1,t2) -> EPair(t1,t2)
      }, eval = {
        case IS(v1,v2) => (v1,v2)
      }
    )

    val collection: IndexedSeq[EFunction] = IS(pair1, pair2, mkPair)

    // examples on how to make a concrete version
    val mkIntPair: EConcreteFunc = mkPair.concretize(IS(EInt, EInt))
  }

  /** add an isolated vertex */
  object GraphComponents {
    val emptyGraph = EAbstractFunc("emptyGraph", tyVarNum = 1,
      typeInstantiation = {
        case IS(eT) => IS() -> EGraph(eT)
      }, eval = {
        case IS() => GraphValue(0, IS())
      })

    val addNode = EAbstractFunc("addNode", tyVarNum = 1,
      typeInstantiation = {
        case IS(eT) => IS(EGraph(eT)) -> EGraph(eT)
      }, eval = {
        case IS(gv: GraphValue) => gv.copy(nodeNum = gv.nodeNum+1)
      })

    /** add an isolated edge */
    val addEdge = EAbstractFunc("addEdge", tyVarNum = 1,
      typeInstantiation = {
        case IS(eT) => IS(EGraph(eT), eT) -> EGraph(eT)
      }, eval = {
        case IS(GraphValue(nodeNum, edges), edgeValue) =>
          val newEdges = edges :+ (nodeNum, nodeNum+1, edgeValue)
          GraphValue(nodeNum + 2, newEdges)
      })

    /** add an edge from/to an existing vertex */
    val growEdge = EAbstractFunc("growEdge", tyVarNum = 1,
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
    val growSelfLoop = EAbstractFunc("growSelfLoop", tyVarNum = 1,
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
    val bridgeEdge = EAbstractFunc("bridgeEdge", tyVarNum = 1,
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
    val deleteEdge = EAbstractFunc("deleteEdge", tyVarNum = 1,
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

    val collection = IS(emptyGraph, addNode, addEdge, growEdge, growSelfLoop, bridgeEdge, deleteEdge)
  }
}
