package com.volhovm.mathlogic.propositional

import scala.language.implicitConversions
import com.volhovm.mathlogic.propositional
import com.volhovm.mathlogic.propositional._
import org.parboiled2._

class ExpressionParser(val input: ParserInput) extends Parser {
  // This works?!
  implicit def wrpStr(s: String): Rule0 = rule {
    zeroOrMore(' ') ~ str(s) ~ zeroOrMore(' ')
  }

  // Grammar
  // S = (A {"," A}* "|-" A ) | A
  def derivationInputLine: Rule1[(List[Expr], Expr)] =
    rule {
      ((zeroOrMore(expression).separatedBy(",") ~> ((a: Seq[Expr]) => a.toList)) ~
      "|-" ~
      expression) ~> ((a: List[Expr], b: Expr) => (a, b)) ~
      EOI }

  def simpleInputLine: Rule1[Expr] = rule { expression ~ EOI }

  private def leftAssoc[A](a: => Rule1[A], b: (A, A) => A, divider: String): Rule1[A]
    = rule { a ~ zeroOrMore(wrpStr(divider) ~ a ~> b) }

  private def expression: Rule1[Expr] = rule {
    oneOrMore(disjunction).separatedBy("->") ~>
    ((a: Seq[Expr]) => a.reduceRight(->)) }
  private def disjunction: Rule1[Expr] = leftAssoc(conjunction, V, "|")
  private def conjunction: Rule1[Expr] = leftAssoc(unary, propositional.&, "&")
  private def unary: Rule1[Expr] =
    rule {
      predicate |
      negate |
      parenth |
      ("@" ~ variable ~ unary ~> ((a, b) => @@(a, b))) |
      ("?" ~ variable ~ unary ~> ((a, b) => ?(a, b))) }
  private def predicate: Rule1[Expr] =
    rule {
      (capture(upper) ~
         optional("(" ~ oneOrMore(term).separatedBy(",") ~ ")") ~>
         ((a, b) => if (b.isEmpty) Pred(a) else Pred(a, b.get: _*))) |
      (term ~ "=" ~ term ~> ((a: Term, b: Term) => propositional.Pred("=", a, b))) }
  def term: Rule1[Term] =
    leftAssoc(summable, ((a: Term, b: Term) => propositional.Term("+", a, b)), "+")
  private def summable: Rule1[Term] =
    leftAssoc(mullable, ((a: Term, b: Term) => Term("*", a, b)), "*")
  private def mullable: Rule1[Term] =
    rule {
      ((capture(lower) ~
         "(" ~
         oneOrMore(term).separatedBy(",") ~
         ")" ~>
         ((a: String, b: Seq[Term]) => Term(a, b: _*)))  |
         variable |
         ("(" ~ term ~ ")") |
         (str("0") ~> (() => Term("0")))) ~
      zeroOrMore(capture("'")) ~> ((a: Term, b: Seq[_]) => wrapInQuote(a, b.length)) }

  private def wrapInQuote(e: Term, n: Int): Term =
    if (n < 1) e else wrapInQuote(Term("'", e), n - 1)
  private def variable: Rule1[Term] =
    rule { (capture(lowerE) | capture(lower)) ~> ((a: String) => Term(a)) }
  private def upper: Rule0 =
    rule { anyOf("PYFGCRLAOEUIDHTNSQJKXBMWVZ") ~ zeroOrMore(anyOf("0123456789")) }
  private def lowerE: Rule0 =
    rule { anyOf("pyfgcrlaoeuidhtnsqjkxbmwvz") ~ zeroOrMore(anyOf("0123456789")) }
  private def lower: Rule0 =
    rule { anyOf("pyfgcrlaoeuidhtnsqjkxbmwvz") }
  private def negate: Rule1[!!] = rule { "!" ~ unary ~> propositional.!! }
  private def parenth: Rule1[Expr] = rule { "(" ~ expression ~ ")" }
}
