package dfa

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should._

class DFASpec extends AnyFunSuite with Matchers:
  test("testDFA accepts the empty word") {
    testDFA.accepts("") should be(true)
  }

  test("testDFA accepts 00") {
    testDFA.accepts("00") should be(true)
  }

  test("testDFA accepts 11") {
    testDFA.accepts("11") should be(true)
  }

  test("testDFA does not accept 0") {
    testDFA.accepts("0") should be(false)
  }

  test("testDFA does not accept 1") {
    testDFA.accepts("1") should be(false)
  }

  test("testDFA does not accept 101") {
    testDFA.accepts("101") should be(false)
  }

end DFASpec
