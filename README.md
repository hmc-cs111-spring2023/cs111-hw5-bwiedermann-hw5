# Scala Machines

_You can work on the assignment on your own or with a partner. I recommend working with a
partner!_

The goal of this assignment is to get practice with Scala's object-oriented and functional
features. Along the way, we will also learn about string-matching with Deterministic
Finite Automata (DFAs) and with Regular Expressions (REs).

These are the parts of the assignment:

0. Set up your development environment
1. Implement string-matching with Deterministic Finite Automata (DFAs)
2. Implement string-matching with Regular Expressions (REs)
3. Commit your code, push it to Github, and submit it on Gradescope.
4. Provide feedback on another assignment.

**Be sure to skim the whole document, before you start coding!**

## Resources

You may need to explore and review a little bit of Scala on your own. In addition to the
resources from class, here are some general resources on learning Scala that might
help:

- [Scala learning overview](http://www.scala-lang.org/documentation/)
- [Scala style guide](http://docs.scala-lang.org/style/)
- [Scala cheatsheet](http://docs.scala-lang.org/cheatsheets/)

Please also ask questions in class, on Discord, and in office hours!

## Part 0: Set up your development environment

As with the previous assignment, there are (at least) three ways to work on the assignment:

1. In Codespaces
2. On your own machine, using Docker
3. On your own machine, using your own installation of Scala

The previous assignment contains instructions for each of these options.

## Part 1: Deterministic Finite Automata (DFAs)

The file [dfa/README.md](dfa/README.md) contains instructions for this part.

## Part 2: Regular Expressions (REs)

The file [regex/README.md](regex/README.md) contains instructions for this part.

## Bonus: Converting REs to DFAs

_There is nothing to do for this part, but just some extra things to check out, now that
you have implemented both DFAs and REs._

Once you have completed both parts, you can try the command `conversion/run` in sbt. This
command runs the `main` program in
[Conversion.scala](conversion/src/main/scala/Conversion.scala). This function converts
an example regular expression to its equivalent DFA, generates a picture of it, and checks
that the original RE and the DFA agree on a sample input.

## Commit, push, and submit to Gradescope

Be sure to remember to commit and push your work to GitHub, and submit on Gradescope! No
matter where you are working (including Codespaces), you will need to push to GitHub, just
as you have for previous assignments.

## Peer-review other people's work

After the submission deadline, I will post critique partners. Each person will add
feedback to someone else's work; though that might mean that some people receive multiple
critiques. You should look through their solutions to each of the parts, and comment on
the clarity of their code. You don't need to comment on everything, but find some
interesting parts of their work to evaluate. Some questions you might consider / answer
when providing feedback:

- How does this team's solution compare to yours? To the provided solution?
  To other people's solutions? Is there anything they did that you like better?
  Is there any way to improve their solution?
- How readable is their code? Is it easy to understand what it does? Is it easy to
  understand how it works?
- How Scala-y is the code? Most of us are just learning the language, and we
  can help each other spot cases when a functional approach would have worked in
  place of an imperative one, etc.
