package com.volhovm.mathlogic

import scala.util.parsing.combinator._

class ExpressionParser[A](varP: String => A, pattern: String = """[A-z]+""") extends JavaTokenParsers {
  // A = B -> A | B
  // B = C {"|" C}*
  // C = D {"&" D}*
  // D = Var | "!" D | "(" A ")"
  def spaces = whiteSpace.?
  def lexem(a: Parser[Expr[A]]) = spaces ~> a <~ spaces
  def parenth(a: Parser[Expr[A]]) = "(" ~> a <~ ")"
  def variable: Parser[Expr[A]] = pattern.r ^^ (a => Var[A](varP(a)))
  def negate: Parser[Expr[A]] = """!""" ~> fourth ^^ { x => Not[A](x)}
  def implication: Parser[Expr[A]] = (second ~ ("->" ~> first)) ^^ { x => Implication[A](x._1, x._2)}

  def first: Parser[Expr[A]] = lexem(implication | second)
  def second: Parser[Expr[A]] = (third ~ ("|" ~> third).*) ^^ { a => a._2.foldLeft(a._1)((x, y) => Or[A](x, y))}
  def third: Parser[Expr[A]] = (fourth ~ ("&" ~> fourth).*) ^^ { a => a._2.foldLeft(a._1)((x, y) => And[A](x, y))}
  def fourth: Parser[Expr[A]] = lexem(variable | negate | parenth(first))

  def getExpression(data: String) = parseAll(first, data).get
}


