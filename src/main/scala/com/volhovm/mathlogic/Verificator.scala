package com.volhovm.mathlogic

/**
 * @author volhovm
 *         Created on 9/10/14
 */

object Verificator {
  // We store Map of [expression, (description, line)] pairs
  type CMap[A] = Map[Expr[A], (Constr, Int)]
  type MPMap[A] = Map[Expr[A], (Expr[A], Int)]
  type MPair[A] = (CMap[A], MPMap[A])
  def emptyPair[A] = (Map.empty[Expr[A], (Constr, Int)], Map.empty[Expr[A], (Expr[A], Int)])

  def verificate[A](exprs: List[Expr[A]],
                    maps: MPair[A] = emptyPair[A],
                    line: Int = 1): CMap[A]
  = if (exprs.isEmpty) maps._1
  else isAxiom(exprs.head, maps, line) match {
    case Some(newMaps) => verificate(exprs.tail, newMaps, line + 1)
    case None => isModusPonens(exprs.head, maps, line) match {
      case Some(newMaps) => verificate(exprs.tail, newMaps, line + 1)
      case None => verificate(exprs.tail, (maps._1.+(exprs.head ->(Fault(), line)), maps._2), line + 1)
    }
  }

  def isModusPonens[A](x: Expr[A], maps: MPair[A] = emptyPair[A], line: Int = 0): Option[MPair[A]]
  = x match {
    case a: -->[A] => maps._2.get(a) match {
      case Some((exp, newLine1)) => maps._1.get(exp) match {
        case Some((description2, newLine2))
        => proceed (maps, line, x, Some(ModusPonens(newLine2, newLine1)))
        case _ => None
      }
      case _ => None
    }
    case _ => None
  }

  def isAxiom[A](x: Expr[A], maps: MPair[A] = emptyPair[A], line: Int = 0): Option[MPair[A]]
  = proceed(maps, line, x, x match {
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
  })

  def proceed[A](maps: MPair[A], line: Int, x: Expr[A], data: Option[Constr]): Option[MPair[A]]
  = data match {
    case Some(construction) => Some(maps._1.+(x ->(construction, line)),
      x match {
        case y: -->[A] => maps._2.+(y.b -> (y.a, line))
        case _ => maps._2
      })
    case _ => None
  }
}
