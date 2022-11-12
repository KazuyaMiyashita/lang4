package lang4.calculator

import org.scalatest.flatspec.AnyFlatSpec

class InterpreterSpec extends AnyFlatSpec {

  import Ast.*

  it should "算術演算の構文木を解釈実行できる(1)" in {
    val interpreter = new Interpreter
    val expr: Ast.Expression = add(integer(10), integer(20))
    val result: Int = interpreter.interpret(expr)
    assert(result == 30)
  }

  it should "算術演算の構文木を解釈実行できる(2)" in {
    val interpreter = new Interpreter

    val expr: Ast.Expression = add(
      subtract(
        integer(1),
        multiply(
          integer(2),
          integer(3)
        )
      ),
      integer(4)
    )

    val result: Int = interpreter.interpret(expr)
    assert(result == -1)
  }

  it should "変数への代入・参照ができる" in {
    val interpreter = new Interpreter

    interpreter.interpret(assignment("foo", integer(42)))
    val result = interpreter.interpret(identifier("foo"))

    assert(result == 42)
  }

  it should "条件分岐を実行できる(条件が真・else句がある)" in {
    val interpreter = new Interpreter

    val expr = _if(
      condition = equal(add(integer(1), integer(2)), integer(3)),
      thenClause = integer(42),
      elseClause = integer(0)
    )

    val result = interpreter.interpret(expr)

    assert(result == 42)
  }

  it should "条件分岐を実行できる(条件が真・else句がない)" in {
    val interpreter = new Interpreter

    val expr = _if(
      condition = equal(add(integer(1), integer(2)), integer(3)),
      thenClause = integer(42)
    )

    val result = interpreter.interpret(expr)

    assert(result == 42)
  }

  it should "条件分岐を実行できる(条件が偽・else句がある)" in {
    val interpreter = new Interpreter

    val expr = _if(
      condition = equal(add(integer(1), integer(2)), integer(99)),
      thenClause = integer(42),
      elseClause = integer(0)
    )

    val result = interpreter.interpret(expr)

    assert(result == 0)
  }

  it should "条件分岐を実行できる(条件が偽・else句がない)" in {
    val interpreter = new Interpreter

    val expr = _if(
      condition = equal(add(integer(1), integer(2)), integer(99)),
      thenClause = integer(42)
    )

    val result = interpreter.interpret(expr)

    assert(result == 1)
  }

  it should "繰り返しを実行できる(0に10回1を加算する)" in {
    val interpreter = new Interpreter

    interpreter.interpret(assignment("i", integer(0)))
    interpreter.interpret(
      _while(
        condition = lessThan(identifier("i"), integer(10)),
        body = assignment("i", add(identifier("i"), integer(1)))
      )
    )
    val result = interpreter.interpret(identifier("i"))

    assert(result == 10)
  }

  it should "複数の式を順次評価する" in {
    val interpreter = new Interpreter

    val expr = block(
      assignment("a", integer(1)),
      assignment("b", integer(2)),
      assignment("c", integer(3)),
      add(add(identifier("a"), identifier("b")), identifier("c"))
    )

    val result = interpreter.interpret(expr)
    assert(result == 6)
  }

  it should "トップレベルの関数定義、グローバル変数を定義して、プラグラムを実行できる" in {
    val interpreter = new Interpreter

    val program: Program = makeProgram(
      defFun(
        "main",
        Nil,
        block(
          callFun("fact", integer(5))
        )
      ),
      defFun(
        "fact",
        List("n"),
        block(
          _if(
            lessThan(identifier("n"), integer(2)),
            integer(1),
            multiply(
              identifier("n"),
              callFun("fact", subtract(identifier("n"), integer(1)))
            )
          )
        )
      )
    )

    val result = interpreter.callMain(program)

    assert(result == 120)
  }

}
