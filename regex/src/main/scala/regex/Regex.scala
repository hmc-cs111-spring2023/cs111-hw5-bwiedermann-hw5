package regex

/** *****************************************************************************
  * Regular Languages
  *
  * data structure definitions for regular languages
  */

/** The supertype of all regular languages * */
trait RegularLanguage

/** Represents ∅ */
object Empty extends RegularLanguage

/** Represents {ε} */
object Epsilon extends RegularLanguage

/** Represents the language that contains a single character c */
case class Character(char: Char) extends RegularLanguage

/** Given L1 and L2, represents L1 ∪ L2 */
case class Union(l1: RegularLanguage, l2: RegularLanguage)
    extends RegularLanguage

/** Given L1 and L2, represents L1 · L2 */
case class Concat(l1: RegularLanguage, l2: RegularLanguage)
    extends RegularLanguage

/** Given L, represents L* */
case class Star(lang: RegularLanguage) extends RegularLanguage

/** *****************************************************************************
  * Derivatives
  *
  * Fill in the function definitions below
  */

/** Simplifies a regular language */
def simplify(lang: RegularLanguage): RegularLanguage = lang match {
  case Concat(Epsilon, l)  => simplify(l)
  case Concat(l, Epsilon)  => simplify(l)
  case Concat(Empty, l)    => Empty
  case Concat(l, Empty)    => Empty
  case Concat(left, right) => Concat(simplify(left), simplify(right))
  case Union(Empty, l)     => simplify(l)
  case Union(l, Empty)     => simplify(l)
  case Union(left, right)  => Union(simplify(left), simplify(right))
  case Star(Epsilon)       => Epsilon
  case Star(Empty)         => Empty
  case Star(lang)          => Star(simplify(lang))
  case _                   => lang
}

/** A language is nullable if it contains ε */
def nullable(lang: RegularLanguage): Boolean = lang match {
  case Epsilon | Star(_) => true
  case Union(a, b)       => nullable(a) || nullable(b)
  case Concat(a, b)      => nullable(a) && nullable(b)
  case _                 => false
}

/** Computes the derivative of a language, with respect to a character */
def derivative(l: RegularLanguage)(c: Char): RegularLanguage = l match {
  // ∂c(∅) = ∅
  // ∂c(ε) = ∅
  case Empty | Epsilon => Empty

  // ∂c({c}) = ε
  // ∂c({d}) = ∅  (if c ≠ d)
  case Character(d) => if (c == d) Epsilon else Empty

  // ∂c(l1 ∪ l2) = ∂c(l1) ∪ ∂c(l2)
  case Union(l1, l2) => Union(derivative(l1)(c), derivative(l2)(c))

  // ∂c(l*) = ∂c(l) · (l*)
  case Star(lang) => Concat(derivative(lang)(c), Star(lang))

  // ∂c(l1 · l2) =
  //    (∂c(l1) · l2)             if ε ∉ l1  (i.e., if l1 is not nullable)
  //    (∂c(l1) ⋅ l2) ∪ ∂c(l2)    otherwise
  case Concat(l1, l2) => {
    val l = Concat(derivative(l1)(c), l2)
    if (!nullable(l1)) then l else Union(l, derivative(l2)(c))
  }
}

/** *****************************************************************************
  * String-matching with regular expressions
  */

/** Given a string s and a language l, returns true if the string matches the
  * pattern described by l
  */
def matches(s: String, l: RegularLanguage): Boolean =
  if (s.isEmpty) then nullable(l)
  else matches(s.tail, derivative(l)(s.head))
