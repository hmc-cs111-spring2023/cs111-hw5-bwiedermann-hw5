# Part 1: Deterministic Finite Automata (DFAs)

In this part of the assignment, we will implement classes that represent a
[Deterministic Finite Automaton
(DFA)](https://en.wikipedia.org/wiki/Deterministic_finite_automaton).[^1] Your classes should
have a specific interface and behavior, as described below. Other than these constraints,
you are free to implement the classes however you like!

[^1] You do not need to know all of the information in the Wikipedia article. It is
just there for your reference!

## Background: DFAs

A DFA is a "machine" that reads in a sequence of symbols (called an **_input_**) and either
accepts or rejects the input. A DFA consists of

- a finite set of **_symbols_**, called the **_alphabet_**
  - the alphabet is the set of all symbols that can appear in the input
- a finite set of **_states_**
  - one of these states is the **_start state_**
  - a subset of these states is the set of **_accepting states_**
- a finite set of **_transitions_**
  - each transition contains three parts: a start state, a symbol that triggers the
    transition, and a state to transition to
  - the set of transitions is complete and deterministic, meaning that for each pair of
    state + symbol, there is exactly one transition

The DFA starts in the start state. As long as there is remaining input, the DFA _consumes_
the next symbol of the input, and then transitions to the state specified by the
transition that matches the current state and the consumed symbol. (Consuming the input
means that the DFA removes that symbol from the start of the input, leaving the suffix of
the input to be read in the future.) When all of the input has been consumed, the DFA
looks at its current state. If that state is one of the accepting states, then the DFA
accepts the input; otherwise, it rejects the input.

## Requirements for implementation

Below are properties that your implementation must have:

- Write your implementation in the file [`dfa/src/main/scala/dfa/DFA.scala`](src/main/scala/dfa/DFA.scala).
- Your implementation should define a **case class** named `State`, with the following
  properties:
  - `label`: a `String` that uniquely identifies the state
- Your implementation should define a **case class** named `Transition`, with the
  following properties:
  - `from`: a `State` that indicates the start state of the transition
  - `symbol`: a `Char` that indicates the symbol that triggers the transition
  - `to`: a `State` that indicates the end state of the transition
- Your implementation should define a **class** named `DFA`, with the following properties:
  - `states`: a `val` of type `Set[State]` that contains all of the states in the DFA
  - `startState`: a `val` of type `State` that indicates the start state of the DFA
  - `acceptingStates`: a `val` of type `Set[State]` that contains all of the accepting
    states in the DFA
  - _Note, we won't store the alphabet in our implementations of DFAs._
  - a method named `accepts` that takes a `String` as input and results in `true` if the
    DFA accepts the input and `false` otherwise.

## Testing the implementation

In the root directory of your repository, run `sbt`.

There are a few things you can do to test your implementation:

- In sbt, compile your code with the command `dfa/compile`. If the code compiles, then
  your interface is correct!
- In sbt, run some provided tests with the command `dfa/test`.
- In sbt, you can run a program that generates a picture of an example DFA. Use the command
  `dfa/run`. If successful, this will generate a file called `output.dot.png`[^2] in
  the root directory of the project. You can open this file to see a picture of the DFA.
  It should look like this:
  
  ![Diagram of DFA](https://hmc-cs111-spring2023.github.io/assets/img/hw5.dot.png)

  _Image description: A picture of a DFA with 2 states. The start state is labeled "Even",
  and it is accepting. The other state is labeled "Odd", and it is not accepting. From
  each state, there are two transitions to the other state. One transition is triggered
  by the symbol "0", and the other transition is triggered by the symbol "1"._

[^2] The `.dot.png` extension indicates that the file is a picture generated from a
[Graphviz](https://en.wikipedia.org/wiki/Graphviz) file. You do not need to know
anything about Graphviz to complete this assignment. But it is a useful DSL for describing
diagrams!
