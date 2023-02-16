package regex

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should._

class MatchesSpec extends AnyFunSuite with Matchers:

  test("ε ∈ bitstring") {
    matches("", bitstring) should be(true)
  }

  test("ε ∈ digits") {
    matches("", digits) should be(true)
  }

  test("ε ∉ digit") {
    matches("", digit) should be(false)
  }

  test("ε ∉ ab*") {
    matches("", re1) should be(false)
  }

  test("ε ∈ (ab)*") {
    matches("", re2) should be(true)
  }

  test("010101 ∈ bitstring") {
    matches("010101", bitstring)
  }

  test("abbb ∈ ab*") {
    matches("abbb", re1)
  }

  test("abbb ∉ (ab)*") {
    matches("abbb", re2)
  }

  test("abab in (ab)*") {
    matches("abab", re2)
  }

end MatchesSpec
