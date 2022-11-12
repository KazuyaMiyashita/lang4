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
          case Operator.Add            => lv + rv
          case Operator.Subtract       => lv - rv
          case Operator.Multiply       => lv * rv
          case Operator.Divide         => lv / rv
          case Operator.LessThan       => if (lv < rv) 1 else 0
          case Operator.LessOrEqual    => if (lv <= rv) 1 else 0
          case Operator.GreaterThan    => if (lv > rv) 1 else 0
          case Operator.GreaterOrEqual => if (lv >= rv) 1 else 0
          case Operator.Equal          => if (lv == rv) 1 else 0
          case Operator.NotEqual       => if (lv != rv) 1 else 0
        }
      case Ast.IntegerLiteral(v) => v
      case Ast.Identifier(name)  => environment(name)
      case Ast.Assignment(name, expression) =>
        interpret(expression).tap(environment.update(name, _))
      case Ast.IfExpression(condition, thenClause, elseClause) =>
        if (interpret(condition) != 0) {
          interpret(thenClause)
        } else {
          elseClause match
            case Some(expr) => interpret(expr)
            case None => 1 // 値は整数しか扱えないため、条件が偽でelse部がない場合は値として1を返すことにする
        }
      case Ast.WhileExpression(condition, body) =>
        while (interpret(condition) != 0) {
          interpret(body)
        }
        1 // 値は整数しか扱えないため、返り値として1を返すことにする
      case Ast.BlockExpression(elements) =>
        var value = 0
        elements.foreach { elem =>
          value = interpret(elem)
        }
        value
    }
  }

}
