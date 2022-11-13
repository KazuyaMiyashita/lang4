package lang4.libparse

import lang4.libparse.Parser.*
import org.scalatest.flatspec.AnyFlatSpec

import scala.annotation.tailrec

class ParserSpec extends AnyFlatSpec {
  import ParserSpec.*

  def parse(in: String): ParseResult[Ast.Expression] = ArithmeticParser.requireTerminal.parse(in)

  it should "算術式をパースできる 0" in {
    import Ast.*
    val result = parse("0")
    val expected = Right(ParseSucceed(IntegerLiteral(0), ""))
    assert(result == expected)
  }

  it should "算術式をパースできる 1+2" in {
    import Ast.*
    val result = parse("1+2")
    val expected = Right(ParseSucceed(BinaryExpression(Operator.Add, IntegerLiteral(1), IntegerLiteral(2)), ""))
    assert(result == expected)
  }

  it should "算術式をパースできる (1+2-3*4)/5" in {
    import Ast.*
    val result = parse("(1+2-3*4)/5")
    val expected = Right(
      ParseSucceed(
        BinaryExpression(
          Operator.Divide,
          BinaryExpression(
            Operator.Subtract,
            BinaryExpression(
              Operator.Add,
              IntegerLiteral(1),
              IntegerLiteral(2)
            ),
            BinaryExpression(
              Operator.Multiply,
              IntegerLiteral(3),
              IntegerLiteral(4)
            )
          ),
          IntegerLiteral(5)
        ),
        ""
      )
    )
    assert(result == expected)
  }

  "listToTree" should "左結合でリストを木に変換できる。すなわち、1, +2, -3 を ((1+2)-3) にする" in {
    import Ast.*
    // 1+2-3 == (1+2)-3 != 1+(2-3)
    val head = IntegerLiteral(1)
    val tail = (Operator.Add, IntegerLiteral(2)) :: (Operator.Subtract, IntegerLiteral(3)) :: Nil
    val result = ArithmeticParser.listToTree(head, tail)
    val expected = BinaryExpression(
      Operator.Subtract,
      BinaryExpression(
        Operator.Add,
        IntegerLiteral(1),
        IntegerLiteral(2)
      ),
      IntegerLiteral(3)
    )
    assert(result == expected)
  }

}

object ParserSpec {

  object Ast {
    sealed trait Operator
    object Operator {
      case object Add extends Operator
      case object Subtract extends Operator
      case object Multiply extends Operator
      case object Divide extends Operator
    }
    sealed trait Expression
    final case class BinaryExpression(operator: Operator, lhs: Expression, rhs: Expression) extends Expression
    final case class IntegerLiteral(value: Int) extends Expression
  }

  object ArithmeticParser extends Parser[Ast.Expression] {

    @tailrec
    def listToTree(head: Ast.Expression, tail: List[(Ast.Operator, Ast.Expression)]): Ast.Expression = {
      tail match {
        case (op, expr) :: next => listToTree(Ast.BinaryExpression(op, head, expr), next)
        case Nil                => head
      }
    }

    def expressionParser: Parser[Ast.Expression] = additiveParser
    def additiveParser: Parser[Ast.Expression] =
      sequence(
        multitiveParser,
        zeroOrMore(
          orderedChoice(
            sequence(atomic("+").map(_ => Ast.Operator.Add), multitiveParser),
            sequence(atomic("-").map(_ => Ast.Operator.Subtract), multitiveParser)
          )
        )
      ).map(listToTree)
    def multitiveParser: Parser[Ast.Expression] =
      sequence(
        primaryParser,
        zeroOrMore(
          orderedChoice(
            sequence(atomic("*").map(_ => Ast.Operator.Multiply), primaryParser),
            sequence(atomic("/").map(_ => Ast.Operator.Divide), primaryParser)
          )
        )
      ).map(listToTree)
    def primaryParser: Parser[Ast.Expression] =
      orderedChoice(
        sequence(atomic("("), expressionParser, atomic(")")).map((_, additive, _) => additive),
        integerParser
      )
    def integerParser: Parser[Ast.Expression] = IntegerLiteralParser.map(Ast.IntegerLiteral.apply)

    override def parse(in: String): ParseResult[Ast.Expression] = expressionParser.parse(in)
  }

}
