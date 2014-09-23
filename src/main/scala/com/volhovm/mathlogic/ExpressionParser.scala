package com.volhovm.mathlogic
import org.parboiled2._

class ExpressionParser(val input: ParserInput) extends Parser {

  // Grammar
  // S = (A {"," A}* "|-" A ) | A
  // A = B "->" A | B
  // B = C {"|" C}*
  // C = D {"&" D}*
  // D = Var | "!" D | "(" A ")"

  def inputLine: Rule1[Expr] = rule { spaces ~ A ~ spaces ~ EOI }
  private def A: Rule1[Expr] = rule { oneOrMore(B).separatedBy("->") ~> ((a: Seq[Expr]) => a.dropRight(1).foldRight(a.last)(-->)) }
//  private def A: Rule1[Expr] = rule { (B ~ "->" ~ A) ~> --> | B }
  private def B: Rule1[Expr] = rule { C ~ zeroOrMore("|" ~ C ~> |||) }
//  private def B: Rule1[Expr] = rule { oneOrMore(C).separatedBy("|") ~> ((a: Seq[Expr]) => a.tail.foldLeft(a.head)(|||)) }
  private def C: Rule1[Expr] = rule { D ~ zeroOrMore("&" ~ D ~> &&&) }
  private def D: Rule1[Expr] = rule { spaces ~ (parenth | variable | negate) ~ spaces }
  private def spaces: Rule0 = rule { zeroOrMore(CharPredicate(" \n\r\t\f")) }
  private def variable: Rule1[Expr] = rule { capture(upper) ~> ((a: String) => Var(a)) }
  private def upper: Rule0 = rule { CharPredicate.UpperAlpha }
  private def negate: Rule1[Expr] = rule { "!" ~ D ~> !! }
  private def parenth: Rule1[Expr] = rule { "(" ~ A ~ ")" }
}


