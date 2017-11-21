package patsyn

case class ProblemConfig(outputTypes: IS[EType],
                         sizeF: PartialFunction[IS[EValue], Int],
                         resourceUsage: PartialFunction[IS[EValue], Double]
                        ) {

}


class SledgeHammer(seed: Long) {

  def genParameters() = ???
}