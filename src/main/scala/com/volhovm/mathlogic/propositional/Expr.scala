package com.volhovm.mathlogic.propositional

/**
 * @author volhovm
 *         Created on 9/10/14
 */

sealed trait Expr {
  def wrap(e: Expr) = e match {
      case !!(_)     => e.toString
      case @@(_, _)  => e.toString
      case ?(_, _)   => e.toString
      case Pred(_)   => e.toString
      case Term(_)   => e.toString
      case _ => "(" + e.toString + ")"
    }
  def arguments[A](str: String, args: Seq[A]) =
    if (args.length > 0) str + "(" + args.mkString(",") + ")" else str

  def string2(divider: String) = { (a: Expr, b: Expr) => wrap(a) + divider + wrap(b) }
  def ->(other: Expr): -> = new ->(this, other)
  def V(other: Expr): V = new V(this, other)
  def &(other: Expr): & = new &(this, other)
}

case class ->(lhs: Expr, rhs: Expr) extends Expr { override def toString = string2("->")(lhs, rhs) }
case class &(lhs: Expr, rhs: Expr) extends Expr { override def toString = string2("&")(lhs, rhs)}
case class V(lhs: Expr, rhs: Expr) extends Expr { override def toString = string2("|")(lhs, rhs)}
case class !!(a: Expr) extends Expr { override def toString: String = "!" + wrap(a) }
case class @@(lhs: Term, rhs: Expr) extends Expr
  { override def toString: String = "∀" + lhs + "." + wrap(rhs) }
case class ?(lhs: Term, rhs: Expr) extends Expr
  { override def toString: String = "∃" + lhs + "." + wrap(rhs) }
case class Pred(name: String, args: Term*) extends Expr
  { override def toString = if (name.length == 1 && !name(0).isLetter && args.length == 2)
                            args(0).toString + name + args(1)
                            else arguments(name, args)
    // this hashcode performs 25% better, I dunno why
   override def hashCode(): Int = name.hashCode + 21 * args.length
  }
case class Term(name: String, args: Term*) extends Expr
  { override def toString: String = if (name.length == 1 && !name(0).isLetter && args.length == 2)
                                      args(0).toString + name + args(1)
                                    else if (name == "'")
                                      args(0).toString + "'"
                                      else  arguments(name, args) }
