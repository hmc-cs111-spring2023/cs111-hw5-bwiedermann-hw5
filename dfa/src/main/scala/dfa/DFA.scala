package dfa // leave this line in the file

/** Represents a DFA state
  *
  * The label is for documentation purposes
  */
case class State(val label: String)

/** Represents a DFA transition
  *
  * @param from
  *   the start state
  * @param to
  *   the new state
  * @param symbol
  *   the symbol that triggers the transition
  */
case class Transition(val from: State, val to: State, val symbol: Char)

/** Represents a DFA
  *
  * @param states
  *   the set of states in the DFA
  * @param transitions
  *   the set of transitions in the DFA
  * @param start
  *   the unique start state
  * @param accept
  *   the (possibly empty) set of accepting states
  */
class DFA(
    val states: Set[State],
    val transitions: Set[Transition],
    val start: State,
    val accept: Set[State]
) {

  /** Run the machine on the input
    *
    * @param input
    *   the string of input symbols
    * @return
    *   true if the machine accepts the input; false otherwise
    */
  def accepts(input: String): Boolean = {
    val finalState = input.foldLeft(start)(findTransition)
    accept.contains(finalState)
  }

  def findTransition(state: State, symbol: Char): State =
    this.transitions.toList.filter(transition =>
      transition.from == state && transition.symbol == symbol
    ) match {
      case List(transition) => transition.to
      case result =>
        throw new Exception(
          s"Invalid transition from state ${state.label} on symbol ${symbol.toString} (${result})"
        )
    }

}
