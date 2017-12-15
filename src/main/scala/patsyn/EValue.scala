package patsyn

trait EValue extends Serializable{
  def hasType(ty: EType): Boolean

  def memoryUsage: Long
}

