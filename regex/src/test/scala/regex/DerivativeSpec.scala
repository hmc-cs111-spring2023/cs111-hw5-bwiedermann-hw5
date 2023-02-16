package regex

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should._

class DerivativeSpec extends AnyFunSuite with Matchers:

  test("derivative ∅ a = ∅") {
    derivative(Empty)('a') should be(
      Empty
    )
  }

  test("derivative ε a = ∅") {
    derivative(Epsilon)('a') should be(
      Empty
    )
  }

  test("derivative a a = ε") {
    derivative(Character('a'))('a') should be(
      Epsilon
    )
  }

  test("derivative a b = ∅") {
    derivative(Character('a'))('b') should be(
      Empty
    )
  }

  test("derivative a ∪ b  a = ε ∪ ∅") {
    derivative(Union(Character('a'), Character('b')))('a') should be(
      Union(Epsilon, Empty)
    )
  }

  test("derivative ab a = εb") {
    derivative(Concat(Character('a'), Character('b')))('a') should be(
      Concat(Epsilon, Character('b'))
    )
  }

  test("derivative ab c = ∅b") {
    derivative(Concat(Character('a'), Character('b')))('c') should be(
      Concat(Empty, Character('b'))
    )
  }

  test("derivative a* a = εa*") {
    derivative(Star(Character('a')))('a') should be(
      Concat(Epsilon, Star(Character('a')))
    )
  }
end DerivativeSpec
