package com.volhovm.mathlogic.propositional

import scala.language.implicitConversions
import com.volhovm.mathlogic.propositional
import com.volhovm.mathlogic.propositional._
import org.parboiled2._

class ExpressionParser(val input: ParserInput) extends Parser {
  // This works?!
  implicit def wspStr(s: String): Rule0 = rule {
    zeroOrMore(' ') ~ str(s) ~ zeroOrMore(' ')
  }

  // Grammar
  // S = (A {"," A}* "|-" A ) | A
  def derivationInputLine: Rule1[(List[Expr], Expr)] =
    rule { ((zeroOrMore(expression).separatedBy(",") ~> ((a: Seq[Expr]) => a.toList)) ~
      "|-" ~
      expression) ~> ((a: List[Expr], b: Expr) => (a, b)) ~
      EOI }

  private def leftAssoc[A, B <: A](a: Rule1[A], b: (A, A) => B, divider: String): Rule1[B]
    = rule { a ~ zeroOrMore(divider ~ a ~> b) }
  // Grammar
  // A = B "->" A | B
  // B = C {"|" C}*
  // C = D {"&" D}*
  // D = Var | "!" D | "(" A ")"
  def simpleInputLine: Rule1[Expr] = rule { expression ~ EOI }
  private def expression: Rule1[Expr] = rule { oneOrMore(disjunction).separatedBy("->") ~>
      ((a: Seq[V]) => a.reduceRight(->)) }
  private def disjunction: Rule1[V] = leftAssoc(conjunction, propositional.V, "V")
  private def conjunction: Rule1[&] = leftAssoc(unary, propositional.&, "&")
  private def unary: Rule1[Expr] =
    rule { predicate | negate | parenth |
            (variable ~ unary ~> ((a, b) => @@(a, b))) |
            ("?" ~ variable ~ unary ~> ((a, b) => ?(a, b))) }
  private def predicate: Rule1[Expr] =
    rule { (capture(upper) ~
              optional("(" ~ oneOrMore(term).separatedBy(",") ~ ")") ~>
              ((a, b) => if (!b.isEmpty) Pred(a) else Pred(a, b.get: _*))) |
            (term ~ "=" ~ term ~> propositional.==) }
  private def term: Rule1[Expr] = leftAssoc(summable, propositional.+, "+")
  private def summable: Rule1[Expr] = leftAssoc(mullable, propositional.*, "*")
  private def mullable: Rule1[Expr] = ??? // переменная -- это нульместный функциональный символ???
  private def variable: Rule1[Var] = rule { capture(lower) ~> ((a: String) => Var(a)) }
  private def upper: Rule0 = rule { anyOf("PYFGCRLAOEUIDHTNSQJKXBMWVZ") ~
      zeroOrMore(anyOf("0123456789")) }
  private def lower: Rule0 = rule { anyOf("pyfgcrlaoeuidhtnsqjkxbmwvz") ~
      zeroOrMore(anyOf("0123456789")) }
  private def negate: Rule1[!!] = rule { "!" ~ unary ~> propositional.!! }
  private def parenth: Rule1[Expr] = rule { "(" ~ expression ~ ")" }
}
