package lang4.calculator

sealed abstract class Operator(val name: String)
object Operator {
  case object Add extends Operator("+")
  case object Subtract extends Operator("-")
  case object Multiply extends Operator("*")
  case object Divide extends Operator("/")
}
