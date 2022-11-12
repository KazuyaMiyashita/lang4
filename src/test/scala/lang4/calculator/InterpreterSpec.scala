package lang4.calculator

import org.scalatest.flatspec.AnyFlatSpec

class InterpreterSpec extends AnyFlatSpec {

  it should "算術演算の構文木を解釈実行できる(1)" in {
    import Ast.*
    val expr: Ast.Expression = add(integer(10), integer(20))
    val result: Int = Interpreter.interpret(expr)
    assert(result == 30)
  }

  it should "算術演算の構文木を解釈実行できる(2)" in {
    import Ast.*
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

    val result: Int = Interpreter.interpret(expr)
    assert(result == -1)
  }

}
