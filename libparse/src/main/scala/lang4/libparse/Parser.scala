package lang4.libparse

import scala.collection.immutable.{AbstractSeq, LinearSeq}

trait Parser[+A] { self =>

  import Parser.*

  def parse(in: String): ParseResult[A]

  /** このパーサーによって終端することを要請する。すなわち消費できなかった文字列があると失敗とする
    * QUESTION: この実装は場当たり的ではないか？
    */
  final def requireTerminal: Parser[A] = { (in: String) =>
    self.parse(in).flatMap { s =>
      Either.cond(
        s.remaining.isEmpty,
        s, {
          val remaining =
            if (s.remaining.length <= 20) { s.remaining }
            else { s.remaining.substring(0, 20) + " ..." }
          ParseFailure(s"Parsing should have been terminated, but the following strings remained: $remaining")
        }
      )
    }
  }

  final def map[A2](f: A => A2): Parser[A2] = { (in: String) =>
    self.parse(in).map { case ParseSucceed(result, remaining) =>
      ParseSucceed(f(result), remaining)
    }
  }

}

object Parser {

  type ParseResult[+A] = Either[ParseFailure, ParseSucceed[A]]
  final case class ParseFailure(message: String)
  final case class ParseSucceed[+A](result: A, remaining: String)

  /** atomic parsing expression
    * 入力文字列の先頭と一致した場合に成功し、その文字列を消費する。一致しなかった場合失敗する。
    */
  def atomic(value: String): Parser[String] = { (in: String) =>
    Either.cond(
      in.startsWith(value),
      ParseSucceed(value, in.substring(value.length)),
      ParseFailure(s"Expected $value, but does not exist.")
    )
  }

  /** 並び
    * PEGの表記で e1 e2 と表されるもの
    * 並び e1 e2 は、まず e1 を呼び出し、 e1 が成功なら続いて e1 が消費した部分を除いて e2 を呼び出し、
    * その結果を全体の結果として返す。
    * e1 か e2 のいずれかが失敗した場合、並び e1 e2 全体が失敗する。
    */
  final def sequence[A1, A2](e1: => Parser[A1], e2: => Parser[A2]): Parser[(A1, A2)] = { (in: String) =>
    for {
      r1 <- e1.parse(in)
      r2 <- e2.parse(r1.remaining)
    } yield ParseSucceed((r1.result, r2.result), r2.remaining)
  }

  final def sequence[A1, A2, A3](e1: => Parser[A1], e2: => Parser[A2], e3: => Parser[A3]): Parser[(A1, A2, A3)] = {
    (in: String) =>
      for {
        r1 <- e1.parse(in)
        r2 <- e2.parse(r1.remaining)
        r3 <- e3.parse(r2.remaining)
      } yield ParseSucceed((r1.result, r2.result, r3.result), r3.remaining)
  }

  final def sequence[A1, A2, A3, A4](
      e1: => Parser[A1],
      e2: => Parser[A2],
      e3: => Parser[A3],
      e4: => Parser[A4]
  ): Parser[(A1, A2, A3, A4)] = { (in: String) =>
    for {
      r1 <- e1.parse(in)
      r2 <- e2.parse(r1.remaining)
      r3 <- e3.parse(r2.remaining)
      r4 <- e4.parse(r3.remaining)
    } yield ParseSucceed((r1.result, r2.result, r3.result, r4.result), r4.remaining)
  }

  /** 選択
    * PEGの表記で e1 / e2 と表されるもの
    * 選択 e1 / e2 は、まず e1 を呼び出し、 e1 が成功ならそれを結果として即座に返す。
    * あるいは e1 が失敗なら入力を e1 を呼び出す前の位置にバックトラッキングして e2 を呼び出し、 e2 の結果を返す。
    */
  final def orderedChoice[A](e1: => Parser[A], e2: => Parser[A]): Parser[A] = { (in: String) =>
    e1.parse(in).orElse(e2.parse(in))
  }

  /** 3つ以上の選択のためのユーティリティ
    * e1 / e2 / ...
    */
  final def orderedChoiceMulti[A](e1: => Parser[A], es: => Parser[A]*): Parser[A] = { (in: String) =>
    es.toList match {
      case e2 :: tail => orderedChoiceMulti(orderedChoice(e1, e2), tail: _*).parse(in)
      case Nil        => e1.parse(in)
    }
  }

  /** 0個以上
    * PEGの表記で e* と表されるもの
    * PEGにおける繰り返しは常に貪欲でありマッチし続ける限り入力を消費するが、それだけではなく、正規表現とは異なりバックトラックしない。
    */
  final def zeroOrMore[A](e: => Parser[A]): Parser[List[A]] = { (in: String) =>
    e.parse(in) match {
      case Left(_) => Right(ParseSucceed(Nil, in)) // パース出来なかった場合、空の結果を成功として返し、入力を消費しない。
      case Right(s1) =>
        val s2: ParseSucceed[List[A]] = zeroOrMore(e).parse(s1.remaining) match {
          case Left(_)      => throw new Exception("unreachable")
          case Right(value) => value
        }
        Right(ParseSucceed(s1.result :: s2.result, s2.remaining))
    }
  }

}
