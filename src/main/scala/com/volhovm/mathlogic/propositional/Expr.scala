package com.volhovm.mathlogic.propositional

/**
 * @author volhovm
 *         Created on 9/10/14
 */

sealed trait Expr {
  def string2(divider: String) = {(a: Any, b: Any) => "(" + a + divider + b + ")"}
  def -->(other: Expr): Expr = new -->(this, other)
  def |||(other: Expr): Expr = new -->(this, other)
  def &&&(other: Expr): Expr = new -->(this, other)
}
case class Var(a: String) extends Expr { override def toString: String = a.toString }
case class -->(lhs: Expr, rhs: Expr) extends Expr { override def toString = string2("->")(lhs, rhs) }
case class &&&(lhs: Expr, rhs: Expr) extends Expr { override def toString = string2("&")(lhs, rhs)}
case class |||(lhs: Expr, rhs: Expr) extends Expr { override def toString = string2("|")(lhs, rhs)}
case class !!(a: Expr) extends Expr { override def toString: String = "!" + a }
//case class |-(context: List[Expr], a: Expr) extends Expr { override def toString: String = context + " |- " + a}