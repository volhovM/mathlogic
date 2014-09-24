package com.volhovm.mathlogic

import scala.annotation.switch
import scala.collection._
import scala.collection.mutable.{MultiMap, HashMap, Set}

/**
 * @author volhovm
 *         Created on 9/10/14
 */

object Verificator {
  // TODO Performance check
  type CMap = Map[Expr, (Constr, Int)]
  type MPMap = HashMap[Expr, Set[(Expr, Int)]] with MultiMap[Expr, (Expr, Int)]
  type State = (CMap, MPMap, List[String])
  private def emptyData = (Map.empty[Expr, (Constr, Int)],
    new HashMap[Expr, Set[(Expr, Int)]] with MultiMap[Expr, (Expr, Int)],
    List[String]())

  def verificate(exprs: List[Expr],
                 margin: Int,
                 state: State = emptyData,
                 line: Int = 1): List[String]
  = if (exprs.isEmpty) state._3
  else verificate(exprs.tail, margin, proceed(state, line, exprs.head, getConstructionType(exprs.head, state), margin), line + 1)

  private def getConstructionType(x: Expr, state: State): Constr
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
    case a if state._1.contains(a) => state._1.get(a).get._1
      // Modus Ponens
    case a: --> => state._2.get(a) match {
      case Some(set) if set.nonEmpty =>
        val (expr, newLine1) = set.reduceRight((a, b) => if (state._1.contains(a._1)) a else b)
        state._1.get(expr) match {
          case Some((_, newLine2)) => ModusPonens(newLine1, newLine2)
          case _ => Fault()
        }
      case _ => Fault()
    }
    case _ => Fault()
  }

  private def proceed(state: State, line: Int, x: Expr, construction: Constr, margin: Int): State
  = (state._1.+(x -> ((construction, line))),
    x match {
      case y: --> =>
        state._2.addBinding(y.b, (y.a, line))
      case _ => state._2
    },
    (line + ". %-" + (margin + 10) + "s%-20s").format(x, construction) :: state._3)
}