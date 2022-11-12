package lang4.calculator

object Interpreter {

  def interpret(expression: Ast.Expression): Int = {
    expression match {
      case Ast.BinaryExpression(operator, lhs, rhs) =>
        val lv: Int = interpret(lhs)
        val rv: Int = interpret(rhs)
        operator match {
          case Operator.Add      => lv + rv
          case Operator.Subtract => lv - rv
          case Operator.Multiply => lv * rv
          case Operator.Divide   => lv / rv
        }
      case Ast.IntegerLiteral(v) => v
    }
  }

}
