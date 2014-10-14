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
  = rule { ((zeroOrMore(A).separatedBy(",") ~> ((a: Seq[Expr]) => a.toList)) ~ "|-" ~ A) ~> ((a: List[Expr], b: Expr) => (a, b)) ~ EOI }

  // Grammar
  // A = B "->" A | B
  // B = C {"|" C}*
  // C = D {"&" D}*
  // D = Var | "!" D | "(" A ")"
  def inputLine: Rule1[Expr] = rule { A ~ EOI }
  private def A: Rule1[Expr] = rule { oneOrMore(B).separatedBy("->") ~> ((a: Seq[Expr]) => a.reduceRight(->)) }
  private def B: Rule1[Expr] = rule { C ~ zeroOrMore("|" ~ C ~> V) }
  private def C: Rule1[Expr] = rule { D ~ zeroOrMore("&" ~ D ~> propositional.&) }
  private def D: Rule1[Expr] = rule { variable | negate | parenth }
  private def variable: Rule1[Expr] = rule { capture(upper) ~> ((a: String) => Var(a)) }
  private def upper: Rule0 = rule { anyOf("ABCPYFGRLOEUIDHTNSQJKXMWVZ") }
  private def negate: Rule1[Expr] = rule { "!" ~ D ~> propositional.¬ }
  private def parenth: Rule1[Expr] = rule { "(" ~ A ~ ")" }
}


