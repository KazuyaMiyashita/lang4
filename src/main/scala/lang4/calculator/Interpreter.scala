package lang4.calculator

import lang4.calculator.Ast.GlobalVariableDefinition

import scala.collection.mutable
import scala.util.chaining.*

class Interpreter {

  var variableEnvironment: Environment[Int] = Environment.empty[Int]

  var functionEnvironment: Environment[Ast.FunctionDefinition] =
    Environment.empty[Ast.FunctionDefinition]

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
      case Ast.Identifier(name)  => variableEnvironment(name)
      case Ast.Assignment(name, expression) =>
        interpret(expression).tap(variableEnvironment.update(name, _))
      case Ast.IfExpression(condition, thenClause, elseClause) =>
        if (interpret(condition) != 0) {
          interpret(thenClause)
        } else {
          elseClause match {
            case Some(expr) => interpret(expr)
            case None       => 1 // 値は整数しか扱えないため、条件が偽でelse部がない場合は値として1を返すことにする
          }
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
      case fnCall: Ast.FunctionCall =>
        functionEnvironment.get(fnCall.name) match {
          case None => throw new LanguageException(s"Function ${fnCall.name} is not found")
          case Some(fnDef) =>
            if (fnDef.args.length != fnCall.args.length) {
              throw new LanguageException(
                s"Function ${fnDef.name} requires ${fnDef.args.length} arguments, but given ${fnCall.args.length} arguments."
              )
            }
            val values: List[Int] = fnCall.args.map(interpret)
            val backup = variableEnvironment
            variableEnvironment = variableEnvironment.newEnvironment
            (fnDef.args zip values).foreach { (name, value) =>
              variableEnvironment.update(name, value)
            }
            val result = interpret(fnDef.body)
            variableEnvironment = backup
            result
        }
    }
  }

  def callMain(program: Ast.Program): Int = {
    program.definitions.foreach {
      case fd: Ast.FunctionDefinition =>
        functionEnvironment.update(fd.name, fd)
      case Ast.GlobalVariableDefinition(name, expression) =>
        variableEnvironment.update(name, interpret(expression))
    }

    functionEnvironment.get("main") match {
      case Some(mainFunction) => interpret(mainFunction.body)
      case None               => throw new LanguageException("This program doesn't have main() function")
    }
  }

}
