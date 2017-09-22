package patsyn

sealed trait EAbstrType

trait EType extends EAbstrType{
}

case class ETyVar(id: Int) extends EAbstrType

object ETyVar{
  implicit def intToTyVar(int: Int): ETyVar = {
    ETyVar(int)
  }
}