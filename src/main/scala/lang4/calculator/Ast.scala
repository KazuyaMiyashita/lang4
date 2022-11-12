package lang4.calculator

object Ast {

  sealed trait Expression
  final case class BinaryExpression(operator: Operator, lhs: Expression, rhs: Expression) extends Expression
  final case class IntegerLiteral(value: Int) extends Expression

  /** 変数の参照を表す */
  final case class Identifier(name: String) extends Expression

  /** 変数の代入を表す */
  final case class Assignment(name: String, expression: Expression) extends Expression

  def add(lhs: Expression, rhs: Expression): BinaryExpression = {
    BinaryExpression(Operator.Add, lhs, rhs)
  }

  def subtract(lhs: Expression, rhs: Expression): BinaryExpression = {
    BinaryExpression(Operator.Subtract, lhs, rhs)
  }

  def multiply(lhs: Expression, rhs: Expression): BinaryExpression = {
    BinaryExpression(Operator.Multiply, lhs, rhs)
  }

  def divide(lhs: Expression, rhs: Expression): BinaryExpression = {
    BinaryExpression(Operator.Divide, lhs, rhs)
  }

  def integer(value: Int): IntegerLiteral = {
    IntegerLiteral(value)
  }

  def identifier(name: String): Identifier = Identifier(name)

  def assignment(name: String, expression: Expression): Assignment = Assignment(name, expression)

}
