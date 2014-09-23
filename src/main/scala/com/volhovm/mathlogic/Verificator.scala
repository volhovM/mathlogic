package com.volhovm.mathlogic

import scala.annotation.switch
import scala.collection._
import scala.collection.mutable.{MultiMap, HashMap, Set}

/**
 * @author volhovm
 *         Created on 9/10/14
 */

object Verificator {
  type CMap = Map[Expr, Int]
  type MPMap = Map[Expr, (Expr, Int)]
  type State = (CMap, MPMap, List[String])
  private def emptyData = (Map.empty[Expr, Int],
    Map.empty[Expr, (Expr, Int)],
    List[String]())

  def verificate(exprs: List[Expr],
                 margin: Int,
                 state: State = emptyData,
                 line: Int = 1): List[String]
  = if (exprs.isEmpty) state._3
  else isAxiom(exprs.head, margin, state, line) match {
    case Some(newMaps) => verificate(exprs.tail, margin, newMaps, line + 1)
    case None => isModusPonens(exprs.head, margin, state, line) match {
      case Some(newMaps) => verificate(exprs.tail, margin, newMaps, line + 1)
      case None => verificate(exprs.tail, margin, (state._1.+(exprs.head -> line), state._2, state._3), line + 1)
    }
  }

  private def isModusPonens(x: Expr, margin: Int, state: State = emptyData, line: Int = 0): Option[State]
  = x match {
    case a: --> => state._2.get(a) match { // FIXME
      case Some((expr, newLine1)) => state._1.get(expr) match {
            case Some(newLine2) => proceed (state, line, x, Some(ModusPonens(newLine2, newLine1)), margin)
            case _ => None
      }
      case _ => None
    }
    case _ => None
  }

  private def isAxiom(x: Expr, margin: Int, state: State = emptyData, line: Int = 0): Option[State]
  = proceed(state, line, x, (x: @switch) match {
    case ((a --> b) --> ((c --> (d --> e)) --> (f --> g)))
    => if (a == c && b == d && e == g && a == f) Some(Axiom(2)) else None
    case ((a --> b) --> ((c --> d) --> ((e ||| f) --> g)))
    => if (a == e && b == d && c == f && d == g) Some(Axiom(8)) else None
    case ((a --> b) --> ((c --> !!(d)) --> !!(e)))
    => if (a == c && b == d && a == e) Some(Axiom(9)) else None
    case (a --> (b --> (c &&& d)))
    => if (a == c && b == d) Some(Axiom(3)) else None
    case ((a &&& b) --> c)
    => if (a == c) Some(Axiom (4)) else if (b == c) Some (Axiom(5)) else None
    case (a --> (b ||| c))
    => if (a == b) Some(Axiom(6)) else if (a == c) Some(Axiom(7)) else None
    case (!!(!!(a)) --> b)
    => if (a == b)  Some(Axiom(10)) else None
    case (a --> (b --> c))
    => if (a == c) Some(Axiom(1)) else None
    case _ => None
  }, margin)

  private def proceed(state: State, line: Int, x: Expr, data: Option[Constr], margin: Int): Option[State]
  = data match {
    case Some(construction) => Some(
      (if (!state._1.contains(x)) state._1.+(x -> line) else state._1,
      x match {
        case y: --> => if (state._1.contains(y.a)) state._2.+(y.b -> (y.a, line)) else state._2
        case _ => state._2
      },
      ("%-" + (margin + 10) + "s%-20s").format(x, construction) :: state._3))
    case _ => None
  }
}
