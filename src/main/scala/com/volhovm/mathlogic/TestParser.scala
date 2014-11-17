package com.volhovm.mathlogic

import com.volhovm.mathlogic.propositional._
import org.parboiled2._

// This is just a class for testing bug in leftAssoc with parameter
// a of type A instead of => A that I used for showing.
// TODO Delete it
class TestParser(val input: ParserInput) extends Parser {
  sealed trait MyExpr
  case class Sum(a: MyExpr, b: MyExpr) extends MyExpr
  case class Mul(a: MyExpr, b: MyExpr) extends MyExpr
  case class Atom(a: String) extends MyExpr

  def leftAssoc[A](a: => Rule1[A], b: (A, A) => A, divider: String): Rule1[A] = rule {
      a ~ zeroOrMore(str(divider) ~ a ~> (b(_: A, _))) }
  def inputLine: Rule1[MyExpr] = rule { sum ~ EOI }
  def sum: Rule1[MyExpr] = rule { mul ~ zeroOrMore("*" ~ mul ~> Mul) }
  def mul: Rule1[MyExpr] = leftAssoc(atom, Sum, "+")
  def atom: Rule1[MyExpr] = rule { (capture(anyOf("PYFGCRLAOEUIDHTNSQJKXBMWVZ")) ~> Atom) |
                                    ("(" ~ sum ~ ")") }
}
