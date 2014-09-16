package com.volhovm.mathlogic

import scala.util.parsing.combinator._

class ExpressionParser[A](varP: String => A, pattern: String = """[A-z]""") extends JavaTokenParsers {
  // Grammar
  // S = (A {"," A}* "|-" A ) | A
  // A = B "->" A | B
  // B = C {"|" C}*
  // C = D {"&" D}*
  // D = Var | "!" D | "(" A ")"
  private def spaces = whiteSpace.?
  private def lexem(a: Parser[Expr[A]]) = spaces ~> a <~ spaces
  private def parenth(a: Parser[Expr[A]]) = "(" ~> a <~ ")"
  private def variable: Parser[Expr[A]] = pattern.r ^^ (a => Var[A](varP(a)))
  private def negate: Parser[Expr[A]] = """!""" ~> D ^^ { x => !![A](x)}
  private def implication: Parser[Expr[A]] = (B ~ ("->" ~> A)) ^^ { x => -->[A](x._1, x._2)}

  def S = ((((A ~ ("," ~> A).*) ^^ { a => a._1 :: a._2 }) ~ ("|-" ~> A)) ^^ {a => |-[A](a._1, a._2)}) | A
  private def A: Parser[Expr[A]] = implication | B
  private def B: Parser[Expr[A]] = (C ~ ("|" ~> C).*) ^^ { a => a._2.foldLeft(a._1)(|||[A])}
  private def C: Parser[Expr[A]] = (D ~ ("&" ~> D).*) ^^ { a => a._2.foldLeft(a._1)(&&&[A])}
  private def D: Parser[Expr[A]] = lexem(variable | negate | parenth(A))

  def getExpression(data: String) = parseAll(S, data).get
}


