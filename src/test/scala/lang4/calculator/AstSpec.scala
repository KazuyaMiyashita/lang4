package lang4.calculator

import org.scalatest.flatspec.AnyFlatSpec

class AstSpec extends AnyFlatSpec {

  it should "構文木を作成できる" in {

    import Ast.*

    add(
      subtract(
        integer(1),
        multiply(
          integer(2),
          integer(3)
        )
      ),
      integer(4)
    )

    succeed
  }

}
