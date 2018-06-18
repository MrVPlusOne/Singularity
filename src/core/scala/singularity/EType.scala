package singularity


/** Concrete Types */
@SerialVersionUID(0)
abstract class EType(val arguments: EType*) extends Serializable {
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

