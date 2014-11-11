package com.volhovm.mathlogic.propositional

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
  def derivationInputLine: Rule1[(List[Expr], Expr)]
  = rule { ((zeroOrMore(expression).separatedBy(",") ~> ((a: Seq[Expr]) => a.toList)) ~ "|-" ~ expression) ~> ((a: List[Expr], b: Expr) => (a, b)) ~ EOI }

  // Grammar
  // A = B "->" A | B
  // B = C {"|" C}*
  // C = D {"&" D}*
  // D = Var | "!" D | "(" A ")"
  def simpleInputLine: Rule1[Expr] = rule { expression ~ EOI }
  private def expression: Rule1[Expr] = rule { oneOrMore(disjunction).separatedBy("->") ~> ((a: Seq[V]) => a.reduceRight(->)) }
  private def disjunction: Rule1[V] = rule { conjunction ~ zeroOrMore("|" ~ conjunction ~> V) }
  private def conjunction: Rule1[&] = rule { unary ~ zeroOrMore("&" ~ unary ~> propositional.&) }
  private def unary: Rule1[Expr] = rule { predicate | negate | parenth | ("@" ~ variable ~ unary ~> @@) | ("?" ~ variable ~ unary ~> ?) }
  private def predicate: Rule1[Expr] = ???
  private def variable: Rule1[Pred] = rule { capture(upper) ~> ((a: String) => Pred(a)) }
  private def upper: Rule0 = rule { anyOf("PYFGCRLAOEUIDHTNSQJKXBMWVZ") ~ zeroOrMore(anyOf("0123456789")) }
  private def lower: Rule0 = rule { anyOf("pyfgcrlaoeuidhtnsqjkxbmwvz") ~ zeroOrMore(anyOf("0123456789")) }
  private def negate: Rule1[!!] = rule { "!" ~ unary ~> propositional.!! }
  private def parenth: Rule1[Expr] = rule { "(" ~ expression ~ ")" }
}


