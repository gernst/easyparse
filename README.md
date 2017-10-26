ARSE Library
============

Libary for *automatic recovery of syntactic elements*.
LL parsing for case classes made simple.

Author: Gidon Ernst <gidonernst@gmail.com>

Feedback is welcome! I'd appreciate to hear whether anyone found this library useful.

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

    val expr: Parser[String, Expr] = P(top)
    val id = string ~> Id
    val app = "(" ~ (expr *) ~ ")" ~> App
    val top = app | id


Use
---

The library interface is divided into `Parser`s and `Recognizer`s
with the distinction that instances of the former produce a value when applied.
In the above example, the `"("` and `")"` are (implicitly) recognizers.
The operator `~` implements sequential composition, it is overloaded to deal
with different combinations of parsers and recognizers and replaces Scala's `~>`
and `<~` combinators.

### Backtracking

The choice combinator `p | q` permits `p` (and `q`) to backtrack as long as they have not yet consumed input.
Otherwise, parse failures are treated as errors. This works well with grammars that need a lookahead of 1.
Sequential composition is strict, which helps error reporting.
You can make a parser or recognizer non-strict by wrapping it in option `p ?` or by `p | parser.Fail`.

### Recursion

Defining a parser `p` as

    val p = P(...)`

has two effects: the argument to `P` is evaluated lazily to support recursion
and `p` implicitly receives the string "p" as its name
(see <https://github.com/lihaoyi/sourcecode>).

Mixfix Operator Parsing
-----------------------

**Note:** currently broken.

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
library for Scala provides a similar interface, but has many additional (cool!) features
and it is designed for speed.

No measurements on efficiency so far, but the library is probably rather slow.
If you need speed, try a LALR parser generator instead, such as
[beaver](http://beaver.sourceforge.net).

TODOs
-----

- transparently hook parse trees into the generation of semantic values
    
    def parse: Tree[A]
    def apply = parse.get
 
- line and position tracking
- mixfix pretty printer