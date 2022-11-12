package lang4.calculator

import scala.collection.mutable
import scala.util.chaining.*

class Interpreter {

  val environment: mutable.Map[String, Integer] = new mutable.HashMap[String, Integer]

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
      case Ast.Identifier(name)  => environment(name)
      case Ast.Assignment(name, expression) =>
        interpret(expression).tap(environment.update(name, _))
    }
  }

}
