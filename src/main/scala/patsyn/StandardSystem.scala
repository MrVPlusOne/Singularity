package patsyn

object StandardSystem {

  // Type declarations
  case object EInt extends EType

  case class EVect(elemT: EType) extends EType

  // Value declarations
  case class IntValue(value: Int) extends EValue {
    def hasType(ty: EType): Boolean = ty == EInt

    override def toString: String = value.toString
  }

  case class VectValue(value: Vector[EValue]) extends EValue{
    def hasType(ty: EType): Boolean = ty match {
      case EVect(et) => value.isEmpty || value.head.hasType(et)
      case _ => false
    }

    override def toString: String = value.mkString("[",",","]")
  }

  implicit def intValue(v: Int): IntValue = IntValue(v)

  implicit def vectValue[E](v: Vector[EValue]): VectValue = {
    VectValue(v)
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
      case IS(e) => IS(EVect(e), EInt) -> e
    }, {
      case IS(VectValue(vec), IntValue(v)) =>
        if (vec.isEmpty) 0
        else {
          val m = v % vec.length
          val i = if (m < 0) m + vec.length else m
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

    val collection: IndexedSeq[EFunction] = IS(append, prepend, access, concat, length)
  }

}
