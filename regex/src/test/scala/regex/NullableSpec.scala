package regex

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should._

class NullableSpec extends AnyFunSuite with Matchers:

  test("ε should be nullable") {
    nullable(Epsilon) should be(true)
  }

  test("∅ should be nullable") {
    nullable(Empty) should be(false)
  }

  test("a should not be nullable") {
    nullable(Character('a')) should be(false)
  }

  test("a* should be nullable") {
    nullable(Star(Character('a'))) should be(true)
  }

  test("ab should not be nullable") {
    nullable(Concat(Character('a'), Character('b'))) should be(false)
  }

  test("aε should not be nullable") {
    nullable(Concat(Character('a'), Epsilon)) should be(false)
  }

  test("εa should not be nullable") {
    nullable(Concat(Epsilon, Character('a'))) should be(false)
  }

  test("εε should be nullable") {
    nullable(Concat(Epsilon, Epsilon)) should be(true)
  }

end NullableSpec
