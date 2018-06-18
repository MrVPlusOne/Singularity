package singularity


/** DSL input type */
@SerialVersionUID(0)
abstract class EType(val arguments: EType*) extends Serializable {
  /** Returns a set of all types that appear within this type */
  def powerset: Set[EType] = {
    if(arguments.isEmpty) Set(this)
    else {
      val argSet = arguments.toSet
      argSet.foldRight(Set(this)){
        case (arg, set) => arg.powerset ++ set
      }
    }
  }
}

/** DSL input value */
@SerialVersionUID(1L)
trait EValue extends Serializable{

  /** should return true if and only if this value has the provided DSL type */
  def hasType(ty: EType): Boolean

  /** Used to prevent badly formed individuals from blowing up memory during the GP
    * fuzzing process. It should be <strong>strictly positive</strong> and proportional to the asymptotic
    * memory usage of the input value. For example, you can set the [[memoryUsage]] of a vector of length n
    * equals to n+1 to satisfy this requirement. */
  def memoryUsage: Long

  /** The cost of this value, should be non-negative. The individual fitness decays exponentially
    * with the total cost of all const values */
  def cost: Double
}
