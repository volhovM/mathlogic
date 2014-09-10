package com.volhovm.mathlogic

/**
 * @author volhovm
 *         Created on 9/10/14
 */

object Verificator {
  // We store Map of [expression, (description, line)] pairs
  type CMap[A] = Map[Expr[A], (String, Int)]
  type MPMap[A] = Map[Expr[A], (Expr[A], Int)]
  type MPair[A] = (CMap[A], MPMap[A])
  def emptyPair[A] = (Map.empty[Expr[A], (String, Int)], Map.empty[Expr[A], (Expr[A], Int)])

  def verificate[A](exprs: List[Expr[A]],
                    maps: MPair[A] = emptyPair[A],
                    line: Int = 1): CMap[A]
  = if (exprs.isEmpty) maps._1
  else isAxiom(exprs.head, maps, line) match {
    case Some(newMaps) => verificate(exprs.tail, newMaps, line + 1)
    case None => isModusPonens(exprs.head, maps, line) match {
      case Some(newMaps) => verificate(exprs.tail, newMaps, line + 1)
      case None => verificate(exprs.tail, (maps._1.+(exprs.head ->("ERROR", line)), maps._2), line + 1)
    }
  }

  def isModusPonens[A](x: Expr[A], maps: MPair[A] = emptyPair[A], line: Int = 0): Option[MPair[A]]
  = x match {
    case a: -->[A] => maps._2.get(a) match {
      case Some((exp, newLine1)) => maps._1.get(exp) match {
        case Some((description2, newLine2))
        => proceed (maps, line, x, (true, "Modus Ponens [" + newLine2 + " " + newLine1 + "]"))
        case _ => None
      }
      case _ => None
    }
    case _ => None
  }

  def isAxiom[A](x: Expr[A], maps: MPair[A] = emptyPair[A], line: Int = 0): Option[MPair[A]]
  = proceed(maps, line, x, x match {
    case -->(-->(a, b), -->(-->(c, -->(d, e)), -->(f, g)))
    => (a == c && b == d && e == g && a == f, "Axiom #2")
    case -->(-->(a, b), -->(-->(c, d), -->(-|(e, f), g)))
    => (a == e && b == d && c == f && d == g, "Axiom #8")
    case -->(-->(a, b), (-->(-->(c, -!(d)), -!(e))))
    => (a == c && b == d && a == e, "Axiom #9")
    case -->(a, -->(b, -&(c, d)))
    => (a == c && b == d, "Axiom #3")
    case -->(-&(a, b), c)
    => (a == c | b == c, "Axiom #4/5")
    case -->(a, -|(b, c))
    => (a == b | a == c, "Axiom #6/7")
    case -->(-!(-!(a)), b)
    => (a == b, "Axiom #10")
    case -->(a, -->(b, c))
    => (a == c, "Axiom #1")
    case _ => (false, "error")
  })

  def proceed[A](maps: MPair[A], line: Int, x: Expr[A], data: (Boolean, String)): Option[MPair[A]]
  = if (data._1)
    Some(maps._1.+(x ->(data._2, line)),
      x match {
        case y: -->[A] => maps._2.+(y.b -> (y.a, line))
        case _ => maps._2
      })
  else None
}
