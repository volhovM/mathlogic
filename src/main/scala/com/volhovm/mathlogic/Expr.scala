package com.volhovm.mathlogic

/**
 * @author volhovm
 *         Created on 9/10/14
 */

sealed trait Expr[A] {
  def string2(divider: String) = {(a: Any, b: Any) => "(" + a + divider + b + ")"}
}
case class Var[A](a: A) extends Expr[A] { override def toString: String = a.toString }
case class -->[A](a: Expr[A], b: Expr[A]) extends Expr[A] { override def toString = string2(" -> ")(a, b) }
case class -&[A](a: Expr[A], b: Expr[A]) extends Expr[A] { override def toString = string2(" & ")(a, b)}
case class -|[A](a: Expr[A], b: Expr[A]) extends Expr[A] { override def toString = string2(" | ")(a, b)}
case class -![A](a: Expr[A]) extends Expr[A] { override def toString: String = "!" + a }