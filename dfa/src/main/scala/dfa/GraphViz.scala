package dfa

import java.io._
import sys.process._

/** Surrounds a given string with quote characters */
def quote(s: String): String = s"\"$s\""

/** Add a graphViz method to states */
def stateToGV(state: State): String = quote(state.label)

/** Add a graphViz method to transitions */
def transitionToGV(transition: Transition): String =
  s"\t${quote(transition.from.label)} -> ${quote(transition.to.label)} [label=${transition.symbol}]"

/** Add a graphViz method to DFAs */
def graphViz(dfa: DFA): String = {
  (
    List(
      "digraph G {",
      "\tnode [shape=circle]",
      s"\t${quote("")} [shape=none]",
      s"\t${quote("")}->${quote(dfa.start.label)}"
    ) ++
      dfa.accept.map(state => s"\t${stateToGV(state)} [shape=doublecircle]") ++
      dfa.transitions.map(transitionToGV) ++
      List("}")
  ).mkString("\n")
}

/** Write a dfa to a file, in graphviz format */
def writeFile(dfa: DFA, filename: String = "./output.dot"): Unit = {
  val file = new File(filename)
  val bw = new BufferedWriter(new FileWriter(file))
  bw.write(graphViz(dfa))
  bw.close()
  s"dot -Tpng $filename -o $filename.png".!
}
