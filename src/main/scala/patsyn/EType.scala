package patsyn


/** Concrete Types */
abstract class EType(val arguments: EType*){
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

