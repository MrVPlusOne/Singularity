package patsyn

@SerialVersionUID(1L)
trait EValue extends Serializable{
  def hasType(ty: EType): Boolean

  def memoryUsage: Long

  /** The cost of this value, should be non-negative. The individual fitness decays exponentially
    * with the total cost of all const values */
  def cost: Double
}

