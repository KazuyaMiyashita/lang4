package lang4.calculator

import Ast.*
import lang4.libparse.Parser.ParseSucceed
import org.scalatest.flatspec.AnyFlatSpec

class Lang4ParserSpec extends AnyFlatSpec {

  it should "プログラムをパースできる (1)" in {
    val str =
      """global i = 0
        |define main() {
        |  i
        |}
        |""".stripMargin

    val result = Lang4Parser.parse(str)
    val expected = Right(
      ParseSucceed(
        makeProgram(
          globalVarDef("i", integer(0)),
          defFun(
            "main",
            Nil,
            block(
              identifier("i")
            )
          )
        ),
        ""
      )
    )

    assert(result == expected)
  }

}
