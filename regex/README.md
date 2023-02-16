# Part 2: Regular Expressions (REs)

[Regular expressions](https://en.wikipedia.org/wiki/Regular_expression)[^2] are an
alternative way to describe DFAs. A regular expression is a pattern that can be used to
match strings. For example, the pattern `a*` matches 0 or more `a`s, and the pattern `a|b`
matches either an `a` or a `b`. A full definition of regular expressions appears below,
along with an algorithm for matching a regular expression against a string. Following that
is a description of the case classes and pattern-matching functions that you will
implement.

In this part of the assignment, we will implement regular expressions and use them to
match strings.

[^2] You do not need to know all of the information in the Wikipedia article. It is just
there for your reference!

## Part 2a: Regular Expressions (REs)

In this part of the assignment, we will write case classes that implement regular expressions.

### Background: Formal Languages and Regular Expressions

First, some definitions: An **_alphabet_** $\Sigma$ is a set of characters. A **_word_**
$w$ is a sequence of characters $c_1⋅⋅⋅c_n$ where $c_i \in \Sigma$; this word has length
$n$. There is a special word called the **_empty word_**, $\epsilon$; this word has length
$0$.

A **_language_** $L$ is a set of words.

Given an alphabet $\Sigma$, the set of **_regular languages_** "over the alphabet" can be
defined inductively as follows:

- The empty language $\emptyset$ is regular.
- The language $\{\epsilon\}$ that contains the empty word is regular.
- If $c \in \Sigma$, then the language $\{c\}$ is regular.
- If $L_1$ and $L_2$ are regular, then the language $L_1 \cup L_2$ is regular.
- If $L_1$ and $L_2$ are regular, then the language $L_1 \cdot L_2$ =
  $\{w_1 \cdot w_2\ |\ w_1 \in L_1 \wedge w_2 \in L_2\}$ is regular. (_The $\cdot$ operator
  is concatenation. Sometimes the operator is omitted, and juxtaposition implies
  concatenation, e.g., `apple` is the concatenation of five characters._)
- If $L$ is regular, then $L^*$ = ${L_n}$ is regular (where
  $L_0 = \{\epsilon\}$ and $L_n = \bigcup_{n=0}^{\infty}(L \cdot L_{n-1})$).

### Implementing regular expressions

In the file [`regex/src/main/scala/regex/Regex.scala`](src/main/scala/regex/Regex.scala),
implement:

- A trait named `RegularLanguage`
- A case class for each of the six forms of regular expressions described above. The names
  of the case classes should be `Empty`, `Epsilon`, `Character`, `Union`, `Concat`, and
  `Star`.

### Testing the implementation

In the root directory of the project, run `sbt`.

In sbt, run the command `regex/compile`. If there are no errors, then you should be good!

## Part 2b: Simplifying regular expressions

Here are some simple rules that can be used to simplify regular expressions. (The function
$s$ is the simplification function.)

$$
\begin{align}
s(\epsilon \cdot L) &=& s(L) \\
s(L \cdot \epsilon) &=& s(L) \\
s(\emptyset \cdot L) &=& s(\emptyset) \\
s(L \cdot \emptyset) &=& s(\emptyset) \\
s(L_1 \cdot L_2) &=& s(L_1) \cdot s(L_2) \\
s(\emptyset \cup L) &=& s(L) \\
s(L \cup \emptyset) &=& s(L) \\
s(L_1 \cup L_2) &=& s(L_1) \cup s(L_2) \\
s(\epsilon^*) &=& \epsilon \\
s({\emptyset}^\*) &=& \emptyset \\
s(L^\*) &=& (s(L))^\* \\
\end{align}
$$

### Implementing simplification

Working in the same file, fill in the a function named `simplify`, which
takes a `RegularLanguage` as input and results in a `RegularLanguage` that is equivalent
to (but possibly simpler than) the input. Use pattern-matching for your implementation,
and be sure to include a fallthrough case that just returns the input.

### Testing the implementation

In the root directory of the project, run `sbt`.

In sbt, run the command `regex/testOnly regex.SimplifySpec`.

## Part 2c: Regular-expression matching

In this part of the assignment, we will write functions that implement regular-expression
matching. First, we describe the algorithm for matching a regular expression against a
string. Then, we describe the pattern-matching functions that you will implement.

### Background: Derivatives

The **_derivative_** of a language $L$ with respect to a character $c \in \Sigma$ is the
language of all suffixes in $L$ of words that start with character $c$. Formally:
$\partial_c(L) = \{w\ |\ c\cdot w \in L\}$. For example, given the language
$L = \{\textrm{apple}\}$ that contains a single word:
$\partial_a(L) = \{\textrm{pple}\}$, and $\partial_q(L) = \emptyset$.

Regular languages are closed under derivatives, which means that the derivative of a
regular language is also a regular language. The derivatives of regular languages are
defined as follows:

- $\partial_c( \emptyset ) = \emptyset$
- $\partial_c( \{\epsilon\} ) = \emptyset$
- $\partial_c( \{d\} ) = \{\epsilon\}$ if $c = d$; otherwise it’s $\emptyset$
- $\partial_c( L_1 \cup L_2 ) = \partial_c( L_1 ) \cup \partial_c( L_2 )$
- $\partial_c( L_1 \cdot L_2 ) = \partial_c( L_1 ) \cdot L_2$ if $\epsilon \not\in L_1$;
  otherwise it’s $(\partial_c( L_1 ) \cdot L_2) \cup \partial_c( L_2 )$
- $\partial_c( L^* ) = \partial_c( L ) \cdot L^*$

### Implementing regular-expression matching

**Task 1**: Working in the same file, fill in the implementation for the function named
`nullable` (see the documentation for what the function should do). Use pattern-matching
for your implementation! After you can complete this part, you can run `regex/testOnly
regex.NullableSpec`.

**Task 2**: Working in the same file, fill in the implementation for the function named
`derivative`, to implement the derivative rules described above. Use pattern-matching for
your implementation! Also, use your implementation of `nullable` when implementing the
derivative for concatenation. After you can complete this part, you can run `regex/testOnly
regex.DerivativeSpec` and `regex/testOnly regex.MatchSpec`.
