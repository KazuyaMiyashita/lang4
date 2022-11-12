package lang4.calculator

import scala.collection.mutable

class Environment[A](
    bindings: mutable.Map[String, A],
    next: Option[Environment[A]]
) {

  def apply(name: String): A = get(name).get

  def get(name: String): Option[A] = findBinding(name).map(_.apply(name))

  def update(name: String, value: A): Unit = {
    findBinding(name) match {
      case Some(binding) => binding.update(name, value)
      case None          => bindings.update(name, value)
    }

  }

  // nextのチェインをだどって、変数のある最も近いマップを返す
  def findBinding(name: String): Option[mutable.Map[String, A]] = {
    if (bindings.isDefinedAt(name)) {
      Some(bindings)
    } else {
      next.flatMap(_.findBinding(name))
    }
  }

  def newEnvironment: Environment[A] = new Environment[A](
    new mutable.HashMap[String, A],
    Some(this)
  )

}

object Environment {

  def empty[A]: Environment[A] = new Environment(new mutable.HashMap[String, A], None)

}
