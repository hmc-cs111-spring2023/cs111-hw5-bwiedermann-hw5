package regex

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should._

class SimplifySpec extends AnyFunSuite with Matchers:

  // a
  val l1 = Character('a')
  // b
  val l2 = Character('b')
  // ab
  val l3 = Concat(l1, l2)
  // aεb
  val l4 = Concat(l1, Concat(Epsilon, l2))

  test("s(εa) = a") {
    simplify(Concat(Epsilon, l1)) should be(l1)
  }

  test("s(aε) = a") {
    simplify(Concat(l1, Epsilon)) should be(l1)
  }

  test("s(∅a) = ∅") {
    simplify(Concat(Empty, l1)) should be(Empty)
  }

  test("s(a∅) = ∅") {
    simplify(Concat(Empty, l1)) should be(Empty)
  }

  test("s(ab) = ab") {
    simplify(l3) should be(l3)
  }

  test("s(aεb) = ab") {
    simplify(l4) should be(l3)
  }

  test("s(∅ ∪ a) = a") {
    simplify(Union(Empty, l1)) should be(l1)
  }

  test("s(a ∪ ∅) = a") {
    simplify(Union(l1, Empty)) should be(l1)
  }

  test("s(a ∪ aεb) = a ∪ ab") {
    simplify(Union(l1, l4)) should be(Union(l1, l3))
  }

  test("s(ε*) = ε") {
    simplify(Star(Epsilon)) should be(Epsilon)
  }

  test("s(∅*) = ∅") {
    simplify(Star(Empty)) should be(Empty)
  }

  test("s((aεb)*) = (ab)*") {
    simplify(Star(l4)) should be(Star(l3))
  }

end SimplifySpec
