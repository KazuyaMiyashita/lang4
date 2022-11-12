package lang4.calculator

import scala.collection.mutable

class Environment(
    bindings: mutable.Map[String, Int],
    next: Option[Environment]
) {

  def findBinding(name: String): Option[mutable.Map[String, Int]] = {
    if (bindings.isDefinedAt(name)) {
      Some(bindings)
    } else {
      next.flatMap(_.findBinding(name))
    }
  }

}
