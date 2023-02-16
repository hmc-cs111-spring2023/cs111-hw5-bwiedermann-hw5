package dfa

// A DFA to use for testing. The DFA accepts all even-length bit-strings.
val testDFA = {
  val s0 = State("Even")
  val s1 = State("Odd")
  val t11 = Transition(s0, s1, '0')
  val t12 = Transition(s0, s1, '1')
  val t01 = Transition(s1, s0, '0')
  val t02 = Transition(s1, s0, '1')

  DFA(
    states = Set(s0, s1),
    transitions = Set(t11, t12, t01, t02),
    start = s0,
    accept = Set(s0)
  )
}

@main
def main() = writeFile(testDFA)
