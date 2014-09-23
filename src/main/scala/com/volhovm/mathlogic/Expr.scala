package com.volhovm.mathlogic

/**
 * @author volhovm
 *         Created on 9/10/14
 */

sealed trait Expr {
  def string2(divider: String) = {(a: Any, b: Any) => "(" + a + divider + b + ")"}
}
case class Var(a: String) extends Expr { override def toString: String = a.toString }
case class -->(a: Expr, b: Expr) extends Expr { override def toString = string2("->")(a, b) }
case class &&&(a: Expr, b: Expr) extends Expr { override def toString = string2("&")(a, b)}
case class |||(a: Expr, b: Expr) extends Expr { override def toString = string2("|")(a, b)}
case class !!(a: Expr) extends Expr { override def toString: String = "!" + a }
case class |-(context: List[Expr], a: Expr) extends Expr { override def toString: String = context + " |- " + a}
