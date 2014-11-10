package com.volhovm.mathlogic.propositional

/**
 * @author volhovm
 *         Created on 9/10/14
 */

sealed trait Expr {
  def wrap(e: Expr) = e match {
      case Var(_) => e.toString
      case !!(_)  => e.toString
      case @@(_)  => e.toString
      case ?(_)   => e.toString
      case _ => "(" + e.toString + ")"
    }
  def string2(divider: String) = { (a: Expr, b: Expr) => wrap(a) + divider + wrap(b) }
  def ->(other: Expr): Expr = new ->(this, other)
  def V(other: Expr): Expr = new V(this, other)
  def &(other: Expr): Expr = new &(this, other)
}

case class Var(a: Char) extends Expr { override def toString: String = a.toUpper.toString }
case class ->(lhs: Expr, rhs: Expr) extends Expr { override def toString = string2("->")(lhs, rhs) }
case class &(lhs: Expr, rhs: Expr) extends Expr { override def toString = string2("&")(lhs, rhs)}
case class V(lhs: Expr, rhs: Expr) extends Expr { override def toString = string2("|")(lhs, rhs)}
case class !!(a: Expr) extends Expr { override def toString: String = "!" + wrap(a) }
case class @@(a: Expr) extends Expr { override def toString: String = "@" + wrap(a) }
case class ?(a: Expr) extends Expr { override def toString: String = "?" ? wrap(a) }
