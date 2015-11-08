ARSE Library
============

Libary for *automatic recovery of syntactic elements*.
LL parsing for case classes made simple.

Author: Gidon Ernst <gidonernst@gmail.com>

Motivation & Overview
---------------------

Scala's case classes are a fine way to represent syntax trees. For example,
consider a simple Lisp-like language for expressions with identifiers, numbers
and function application:

    trait Expr
    case class Id(name: String) extends Expr
    case class Num(value: Int) extends Expr
    case class App(args: List[Expr]) extends Expr

This definition corresponds (almost) directly to the following attributed grammar:

    expr := id | num | app
    id   := string        { Id(_)  }
    num  := int           { Num(_) }
    app  := "(" expr* ")" { App(_) }

The aim of this library is to minimize the fuss to create a parser for such a
language. The main idea is that, given a parser for the arguments of a case
class constructor, the parser for the case class is instantiated automatically.
With *arse* you can specify this as follows:

    implicit val expr_r = rec(expr)

    val id  = parse(Id)
    val num = parse(Num)
    val app = lit("(") ~> parse(App) <~ lit(")") 
    val expr = app | num | id

The method `parse` takes a function as argument and for each of its parameter
types an implicit parameter to specify the corresponding parser.
For instance:

    def parse[A,R](f: A => R)(implicit p: Parser[A]) = p map f

where `p map f` denotes applying `f` to the result of `p`.

The library already provides parsers for the primitive types such as `String`,
`Boolean`, and `Int` alongside lifting of parsers to options and lists
(as required for the production `app`).

Recursion is implemented in the `rec` combinator, which defers evaluation of its
argument. Note that `expr_r` is an implicit value, so that the production `app`
can pick it up automatically.

Implementation
--------------

Trait `Combinators` is extended to use parsers. One has to specify the abstract
type `T`, which corresponds to `Elem` in Scala's parser combinator library.
Two canonical choices are `String` and `arse.Token`.

A `Parser[T,+A]` is essentially a function `Input => (A, Input)`,
where `type Input = List[T]`.

Parsers are allowed to backtrack, which is implemented via non-local goto using
exceptions of type `Backtrack`. As an example, the choice combinator roughly
corresponds to

    (p1 | p2)(in) = p1(in) or p2(in)

where `or` executes its right argument only iff its left arguments throws a
`Backtrack`.

Reference
---------

Terminals/atomic parsers

- `__`: accept and return the next token unconditionally
- `lit(t: T)`:  accept and return token `t` if `t` is at the current position in the input
- `lit(s: String)`: accept a token `t` with `t.toString == s` and return `s`
- `lit[A](t: T, a: A)`:  accept `t` and return `a`
- `lit[A](s: String, a: A)`: similarly
- `ret[A](a: A)` return `a` without consuming any input
- `next f`: equivalent to `__ map f` (see below)

Compound parsers

- `rec(p)`: defer evaluation of `p` to its first use, for recursive parsers
- `p ?`: try to parse `p`, return an `Option`
- `p *`: greedily parse zero or more occurrences of `p`, returning a `List`
- `p +`: greedily parse one or more occurrences of `p`, returning a `List`
- `p1 | p2`: choice with preference for `p1`
- `p1 ~ p2`: sequential composition returning a standard Scala pair
- `p1 ~> p2`: sequential composition discarding the result of `p1`
- `p1 <~ p2`: sequential composition discarding the result of `p2`
- `p1 >> fp2`: feed the result of `p1` to `fp2: A => Parser[B]`
- `p map f`: map the result of `p` by function `f`
- `p foreach f`: similar to `map` but without result
- `p collect f`: similar to `map` but with partial function `f`
- `p filter[Not] f`: test result of `p` by predicate `f`, fail if it is not desired
- `p ! msg`: make `p` strict, failure is turned into an error with a given message `msg`
- `p $`: force end of input
- `repsep(p, q)`: parse a sequence of `p` separated by `p`

Not implemented yet

- `repsep1(p, q)`: as above but guarantee at least one occurence of `p`

Predefined parsers:

- `trait Primitive`: implicit parsers for primitive Scala values
     `Boolean`, `String`, `Int`, `Long`, `Float`, and `Double`
- `trait Collection`: implicit parsers for `List` and `Option`

Constructing parsers for case classes:

- `parse(f: (A1, ..., An) => R)`: lift a function `f` with `n` arguments to a
  parser; given parsers for `A1, ..., An` as implicit parameters

Non-local control flow, `import arse.control._`

- `fail`: canonical backtrack
- `a or b`: try `a`, if it throws `Backtrack`, try `b` instead (otherwise `b` is
  not evaluated)

Mixfix Operator Parsing
-----------------------

The trait `Mixfix` implements
[Pratt](https://en.wikipedia.org/wiki/Pratt_parser)-style parsers.
It has the following parameters

- `type Op`: the type of operators
- `type Expr`: the type of expressions
- `pre/post/infix_op`:
  parse an operator of the given kind, returning its precedence
  (and associativity)
- `inner_expr`: anything that binds stronger than mixfix operators
- `unary` and `binary` expression constructors

Note that it is possible to overload operators in the different categories,
the parsing algorithm can discern e.g. between unary and binary `-` (minus).
Postfix takes precedence over infix,
i.e., parsing repetition `*` will precede binary multiplication.

See also:

- <http://javascript.crockford.com/tdop/tdop.html>
- <http://www.engr.mun.ca/~theo/Misc/exp_parsing.htm>

Limitations
-----------

The [combinator](https://github.com/scala/scala-parser-combinators)
library (no longer) shipped with Scala is much more feature complete and supports
left recursion by packrat parsing.

No measurements on efficiency so far, but the library is probably rather slow.
If you need speed, try a LALR parser generator instead, such as
[beaver](http://beaver.sourceforge.net).

No scanner functionality is provided. [jFlex](http://jflex.de) is really nice.
