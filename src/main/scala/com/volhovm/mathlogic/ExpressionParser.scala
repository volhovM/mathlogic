package com.volhovm.mathlogic

import scala.util.parsing.combinator._

class ExpressionParser[A](varP: String => A, pattern: String = """[A-z]""") extends JavaTokenParsers {
  // Grammar
  // A = B -> A | B
  // B = C {"|" C}*
  // C = D {"&" D}*
  // D = Var | "!" D | "(" A ")"
  def spaces = whiteSpace.?
  def lexem(a: Parser[Expr[A]]) = spaces ~> a <~ spaces
  def parenth(a: Parser[Expr[A]]) = "(" ~> a <~ ")"
  def variable: Parser[Expr[A]] = pattern.r ^^ (a => Var[A](varP(a)))
  def negate: Parser[Expr[A]] = """!""" ~> D ^^ { x => -![A](x)}
  def implication: Parser[Expr[A]] = (B ~ ("->" ~> A)) ^^ { x => -->[A](x._1, x._2)}

  def A: Parser[Expr[A]] = lexem(implication | B)
  def B: Parser[Expr[A]] = (C ~ ("|" ~> C).*) ^^ { a => a._2.foldLeft(a._1)((x, y) => -|[A](x, y))}
  def C: Parser[Expr[A]] = (D ~ ("&" ~> D).*) ^^ { a => a._2.foldLeft(a._1)((x, y) => -&[A](x, y))}
  def D: Parser[Expr[A]] = lexem(variable | negate | parenth(A))

  def getExpression(data: String) = parseAll(A, data).get
}


