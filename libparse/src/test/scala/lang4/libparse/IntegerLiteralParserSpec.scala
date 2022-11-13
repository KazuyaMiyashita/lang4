package lang4.libparse

import lang4.libparse.Parser.*
import org.scalatest.flatspec.AnyFlatSpec

class IntegerLiteralParserSpec extends AnyFlatSpec {

  def parse(in: String): ParseResult[Int] = IntegerLiteralParser.requireTerminal.parse(in)

  it should "0 をパースできる" in {
    assert(parse("0") == Right(ParseSucceed(0, "")))
  }

  it should "42 をパースできる" in {
    assert(parse("42") == Right(ParseSucceed(42, "")))
  }

  it should "-123 をパースできる" in {
    assert(parse("-123") == Right(ParseSucceed(-123, "")))
  }

  it should "-0 はパースに失敗する" in {
    assert(parse("-0") == Left(ParseFailure("Expected 9, but does not exist.")))
  }

  it should "003 はパースに失敗する" in {
    assert(
      parse("003") == Left(ParseFailure("Parsing should have been terminated, but the following strings remained: 03"))
    )
  }

}
