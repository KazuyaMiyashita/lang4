package lang4.calculator

import Ast.*
import lang4.libparse.Parser.ParseSucceed
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.flatspec.AnyFlatSpec

class Lang4Spec extends AnyFlatSpec {

  def run(str: String): Int = {
    new Interpreter().callMain(Lang4Parser.parse(str) match {
      case Left(value)  => throw new Exception(s"Parse Failure: ${value.message}")
      case Right(value) => value.result
    })
  }

  it should "プログラムをパースし実行できる" in {
    val str =
      """define factorial(n) {
        |  if (n < 2) {
        |    1;
        |  } else {
        |    n * factorial(n - 1);
        |  }
        |}
        |define main() {
        |  factorial(5);
        |}
        |""".stripMargin

    assert(run(str) == 120)
  }

}
