ARSE Library
============

Libary for *automatic recovery of syntactic elements*.
LL parsing for case classes made simple.

Author: Gidon Ernst <gidonernst@gmail.com>

Feedback is welcome! I'd appreciate to hear whether anyone found this library useful.

**Note:** this library underwent lots of refactoring and cleanups recently.
Refer to branch [v0](https://github.com/gernst/arse/tree/v0) for further infos
and documentation.

**Hot:** Bidirectional parsing and printing in package
[`arse.bi`](https://github.com/gernst/arse/tree/master/src/arse/bi)
(no documentation yet). See `Test.scala` for an example.

Motivation & Overview
---------------------

Scala's case classes are a fine way to represent syntax trees. For example,
consider a simple Lisp-like language for expressions with identifiers, numbers
and function application:

    trait Expr
    case class Id(name: String) extends Expr
    case class App(args: List[Expr]) extends Expr

This definition corresponds (almost) directly to the following attributed grammar:

    expr := id | app
    id   := string        { Id(_)  }
    app  := "(" expr* ")" { App(_) }

The aim of this library is to minimize the fuss to create a parser for such a
language. The main idea is that, given a parser for the arguments of a case
class constructor, the parser for the case class is instantiated automatically.
With *arse* you can specify this as follows:

    val expr: Parser[String, Expr] = Parser.rec(top)
    val id = Id.from(string)
    val app = "(" ~ App.from(expr *) ~ ")"
    val top = app | id

The method `.from` extends function values and takes as argument and for each of
its parameter types a parameter to specify the corresponding parser.
Recursion is implemented in the `rec` combinator,
which defers evaluation of its argument.

Implementation
--------------

The library interface is divided into `Parser`s and `Recognizer`s
with the distinction that instances of the former produce a value when applied.
In the above example, the `"("` and `")"` are (implicitly) recognizers.
The operator `~` implements sequential composition, it is overloaded to deal
with different combinations of parsers and recognizers and replaces Scala's `~>`
and `<~` combinators.

Parsers and recogniziers are allowed to backtrack, which is implemented via
non-local goto using exceptions of type `Backtrack`.
As an example, the choice combinator roughly corresponds to

    (p1 | p2)(in) = p1(in) or p2(in)

where `or` executes its right argument only iff its left arguments throws a
`Backtrack`.

Mixfix Operator Parsing
-----------------------

The trait `Mixfix` implements
[Pratt](https://en.wikipedia.org/wiki/Pratt_parser)-style parsers.
It has the following parameters

- `type O`: the type of operators
- `type E`: the type of expressions
- `pre/post/infix_op`:
  parse an operator of the given kind, returning its precedence
  (and associativity)
- `inner_expr`: anything that binds stronger than mixfix operators
- `unary` and `binary` expression constructors

The companion object provides a method `mixfix` to instantiate such a parser,
given constructors for operators and application, and a `Syntax` object defining
the mixfix operators. The first parameter `p` of `mixfix` is used to parse
atomic inner terms.

Note that it is possible to overload operators in the different categories,
the parsing algorithm can discern e.g. between unary and binary `-` (minus).
Infix takes precedence over postfix,
i.e., parsing repetition `*` will precede binary multiplication
*only* if the postfix priority of `*` is high enough to prohibit a right
argument (at the given source location).

See also:

- <http://javascript.crockford.com/tdop/tdop.html>
- <http://www.engr.mun.ca/~theo/Misc/exp_parsing.htm>
- <http://www.cs.nott.ac.uk/~pszgmh/monparsing.pdf> (about the Parser Monad)
- Project [ulang](https://github.com/gernst/ulang) for a larger example
  (`source/Parser.scala` in branch *master*
   resp. `source/Grammar.scala` in *refactor-parser*)

Limitations
-----------

The [combinator](https://github.com/scala/scala-parser-combinators)
library (no longer) shipped with Scala is much more feature complete and supports
left recursion by packrat parsing.

The [fastparse](http://www.lihaoyi.com/fastparse)
library for Scala provides a similar interface, but has many additional (cool!)
features.

No measurements on efficiency so far, but the library is probably rather slow.
If you need speed, try a LALR parser generator instead, such as
[beaver](http://beaver.sourceforge.net).

No scanner functionality is provided. [jFlex](http://jflex.de) is really nice.
