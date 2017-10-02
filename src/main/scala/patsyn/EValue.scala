package patsyn

trait EValue {
  def hasType(ty: EType): Boolean

  def size: Long
}

