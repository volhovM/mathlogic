package com.volhovm.mathlogic.propositional

import scala.annotation.switch
import scala.collection._
import scala.collection.mutable.{HashMap, MultiMap, Set}

/**
 * @author volhovm
 *         Created on 9/10/14
 */

object Annotator {
  private type State[A] = (CMap, MPMap, Context, List[A])
  private type CMap = Map[Expr, (Constr, Int)]
  private type MPMap = HashMap[Expr, Set[(Expr, Int)]] with MultiMap[Expr, (Expr, Int)]

  def contextState[A](input: Context) = (
    Map.empty[Expr, (Constr, Int)],
    new HashMap[Expr, Set[(Expr, Int)]] with MultiMap[Expr, (Expr, Int)],
    input,
    List[A]())

  def emptyState[A]: State[A] = contextState[A](List[Expr]())

  def annotateGeneric[A](exprs: Proof,
                  state: State[A],
                  wrapper: (Expr, Constr, Int) => A,
                  line: Int = 0): List[A] = 
    if (exprs.isEmpty) state._4.reverse
    else annotateGeneric(exprs.tail, wrap(state, line, exprs.head, getConstructionType(exprs.head, state), wrapper), wrapper, line + 1)
  
  def annotate[A](exprs: Proof, state: State[A] = emptyState[A]) = annotateGeneric[(Expr, Constr)](exprs, emptyState[(Expr, Constr)], {(e, c, i) => (e, c)})
  
  def annotateLined(exprs: Proof, state: State[(Int, Expr, Constr)] = emptyState[(Int, Expr, Constr)]) =
    annotateGeneric(exprs, state, {(x, construction, line) => (line, x, construction)})
  
  def annotateString(exprs: Proof, margin: Int, state: State[String] = emptyState[String]) =
    annotateGeneric(exprs, emptyState[String], {(x, construction, line) => (line + ". %-" + (margin + 10) + "s%-20s").format(x, construction)})

  private def getConstructionType[A](x: Expr, state: State[A]): Constr
  = (x: @switch) match {
      // Axioms
    case ((a --> b) --> ((c --> (d --> e)) --> (f --> g))) if a == c && b == d && e == g && a == f => Axiom(2)
    case ((a --> b) --> ((c --> d) --> ((e ||| f) --> g))) if a == e && b == d && c == f && d == g => Axiom(8)
    case ((a --> b) --> ((c --> !!(d)) --> !!(e))) if a == c && b == d && a == e => Axiom(9)
    case (a --> (b --> (c &&& d))) if a == c && b == d => Axiom(3)
    case ((a &&& b) --> c) if a == c => Axiom(4)
    case ((a &&& b) --> c) if b == c => Axiom(5)
    case (a --> (b ||| c)) if a == b => Axiom(6)
    case (a --> (b ||| c)) if a == c => Axiom(7)
    case (!!(!!(a)) --> b) if a == b => Axiom(10)
    case (a --> (b --> c)) if a == c => Axiom(1)
      // Repeating
//    case a if state._1.contains(a) => state._1.get(a).get._1
      // Assumption
    case a if state._3.contains(a) => Assumption()
      // Modus Ponens
    case a: --> => state._2.get(a) match {
      case Some(set) if set.nonEmpty =>
        val (expr, newLine1) = set.reduceRight((a, b) => if (state._1.contains(a._1)) a else b)
        state._1.get(expr) match {
          case Some((_, newLine2)) => ModusPonens(newLine2, newLine1) // WHAT, INTO WHAT
          case _ => Fault()
        }
      case _ => Fault()
    }
    case _ => Fault()
  }

  private def wrap[A](state: State[A], line: Int, x: Expr, construction: Constr, wrapper: (Expr, Constr, Int) => A): State[A]
  = (state._1.+(x -> ((construction, line))),
    x match {
      case y: --> =>
        state._2.addBinding(y.rhs, (y.lhs, line))
      case _ => state._2
    },
    state._3,
    wrapper(x, construction, line) :: state._4)
}