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

  // not working
  def leftAssoc[A](a: => Rule1[A], b: (A, A) => A, divider: String): Rule1[A]
    = rule { a ~ zeroOrMore(str(divider) ~ a ~> b) }

  def simpleInputLine: Rule1[Expr] = rule { expression ~ EOI }
   def expression: Rule1[Expr] = rule { oneOrMore(disjunction).separatedBy("->") ~>
      ((a: Seq[Expr]) => a.reduceRight(->)) }
   def disjunction: Rule1[Expr] = leftAssoc(conjunction, V, "|")
//     leftAssoc[Expr](conjunction, propositional.V, "V")
   def conjunction: Rule1[Expr] = leftAssoc(unary, propositional.&, "&")
   def unary: Rule1[Expr] =
    rule { predicate | negate | parenth |
            (variable ~ unary ~> ((a, b) => @@(a, b))) |
            ("?" ~ variable ~ unary ~> ((a, b) => ?(a, b))) }
   def predicate: Rule1[Expr] =
    rule { (capture(upper) ~
              optional("(" ~ oneOrMore(term).separatedBy(",") ~ ")") ~>
              ((a, b) => if (b.isEmpty) Pred(a) else Pred(a, b.get: _*))) |
            (term ~ "=" ~ term ~> ((a: Term, b: Term) => propositional.Pred("=", a, b))) }
   def term: Rule1[Term] =
//     rule { summable ~ zeroOrMore("+" ~ summable ~> ((a: Term, b: Term) => Term("+", a, b)))}
    leftAssoc(summable, ((a: Term, b: Term) => propositional.Term("+", a, b)), "+")
   def summable: Rule1[Term] =
//     rule { mullable ~ zeroOrMore("*" ~ mullable ~> ((a: Term, b: Term) => Term("*", a, b))) }
     leftAssoc(mullable, ((a: Term, b: Term) => Term("*", a, b)), "*")
   def mullable: Rule1[Term] =
    rule { capture(lower) ~ "(" ~ oneOrMore(term).separatedBy(",") ~ ")" ~> ((a: String, b: Seq[Term]) => Term(a, b: _*))  |
            variable |
            ("(" ~ term ~ ")") |
            (ch('0') ~> (() => Term("0"))) }
//            ( mullable ~ "'" ~> ((a: Term) => Term("'", a)))}
   def variable: Rule1[Term] = rule { capture(lower) ~> ((a: String) => Term(a)) }
   def upper: Rule0 = rule { anyOf("PYFGCRLAOEUIDHTNSQJKXBMWVZ") ~
      zeroOrMore(anyOf("0123456789")) }
   def lower: Rule0 = rule { anyOf("pyfgcrlaoeuidhtnsqjkxbmwvz") ~
      zeroOrMore(anyOf("0123456789")) }
   def negate: Rule1[!!] = rule { "!" ~ unary ~> propositional.!! }
   def parenth: Rule1[Expr] = rule { "(" ~ expression ~ ")" }
}
