package com.volhovm.mathlogic.propositional

/**
 * @author volhovm
 *         Created on 10/6/14
 */

object Proofs {
  def ident(a: Expr): Proof =
    List[Expr](
      a -> (a -> a),
      (a -> (a -> a)) -> ((a -> ((a -> a) -> a)) -> (a -> a)),
      a -> ((a -> a) -> a),
      (a -> ((a -> a) -> a)) -> (a -> a),
      a -> a
    )

  def deduction1(a: Expr, alpha: Expr): Derivation =
    (List(a), List[Expr](
      a,
      a -> (alpha -> a),
      alpha -> a
    ))

  def deduction2(a: Expr, alpha: Expr, e1: Expr, e2: Expr): Proof =
    List[Expr](
      (alpha -> e1) -> ((alpha -> (e1 -> a)) -> (alpha -> a)),
      (alpha -> (e1 -> a)) -> (alpha -> a),
      alpha -> a
    )

  def contraposition(a: Expr, b: Expr): Proof = List[Expr](
    (!!(b) -> (a -> !!(b))) -> ((!!(b) -> ((a -> !!(b)) -> !!(a))) -> (!!(b) -> !!(a))),
    ((!!(b) -> (a -> !!(b))) -> ((!!(b) -> ((a -> !!(b)) -> !!(a))) -> (!!(b) -> !!(a)))) -> ((a -> b) -> ((!!(b) -> (a -> !!(b))) -> ((!!(b) -> ((a -> !!(b)) -> !!(a))) -> (!!(b) -> !!(a))))),
    (a -> b) -> ((!!(b) -> (a -> !!(b))) -> ((!!(b) -> ((a -> !!(b)) -> !!(a))) -> (!!(b) -> !!(a)))),
    !!(b) -> (a -> !!(b)),
    (!!(b) -> (a -> !!(b))) -> ((a -> b) -> (!!(b) -> (a -> !!(b)))),
    (a -> b) -> (!!(b) -> (a -> !!(b))),
    ((a -> b) -> (!!(b) -> (a -> !!(b)))) -> (((a -> b) -> ((!!(b) -> (a -> !!(b))) -> ((!!(b) -> ((a -> !!(b)) -> !!(a))) -> (!!(b) -> !!(a))))) -> ((a -> b) -> ((!!(b) -> ((a -> !!(b)) -> !!(a))) -> (!!(b) -> !!(a))))),
    ((a -> b) -> ((!!(b) -> (a -> !!(b))) -> ((!!(b) -> ((a -> !!(b)) -> !!(a))) -> (!!(b) -> !!(a))))) -> ((a -> b) -> ((!!(b) -> ((a -> !!(b)) -> !!(a))) -> (!!(b) -> !!(a)))),
    (a -> b) -> ((!!(b) -> ((a -> !!(b)) -> !!(a))) -> (!!(b) -> !!(a))),
    (!!(b) -> (a -> b)) -> ((!!(b) -> ((a -> b) -> ((a -> !!(b)) -> !!(a)))) -> (!!(b) -> ((a -> !!(b)) -> !!(a)))),
    ((!!(b) -> (a -> b)) -> ((!!(b) -> ((a -> b) -> ((a -> !!(b)) -> !!(a)))) -> (!!(b) -> ((a -> !!(b)) -> !!(a))))) -> ((a -> b) -> ((!!(b) -> (a -> b)) -> ((!!(b) -> ((a -> b) -> ((a -> !!(b)) -> !!(a)))) -> (!!(b) -> ((a -> !!(b)) -> !!(a)))))),
    (a -> b) -> ((!!(b) -> (a -> b)) -> ((!!(b) -> ((a -> b) -> ((a -> !!(b)) -> !!(a)))) -> (!!(b) -> ((a -> !!(b)) -> !!(a))))),
    (a -> b) -> (!!(b) -> (a -> b)),
    ((a -> b) -> (!!(b) -> (a -> b))) -> (((a -> b) -> ((!!(b) -> (a -> b)) -> ((!!(b) -> ((a -> b) -> ((a -> !!(b)) -> !!(a)))) -> (!!(b) -> ((a -> !!(b)) -> !!(a)))))) -> ((a -> b) -> ((!!(b) -> ((a -> b) -> ((a -> !!(b)) -> !!(a)))) -> (!!(b) -> ((a -> !!(b)) -> !!(a)))))),
    ((a -> b) -> ((!!(b) -> (a -> b)) -> ((!!(b) -> ((a -> b) -> ((a -> !!(b)) -> !!(a)))) -> (!!(b) -> ((a -> !!(b)) -> !!(a)))))) -> ((a -> b) -> ((!!(b) -> ((a -> b) -> ((a -> !!(b)) -> !!(a)))) -> (!!(b) -> ((a -> !!(b)) -> !!(a))))),
    (a -> b) -> ((!!(b) -> ((a -> b) -> ((a -> !!(b)) -> !!(a)))) -> (!!(b) -> ((a -> !!(b)) -> !!(a)))),
    ((a -> b) -> ((a -> !!(b)) -> !!(a))) -> (!!(b) -> ((a -> b) -> ((a -> !!(b)) -> !!(a)))),
    (((a -> b) -> ((a -> !!(b)) -> !!(a))) -> (!!(b) -> ((a -> b) -> ((a -> !!(b)) -> !!(a))))) -> ((a -> b) -> (((a -> b) -> ((a -> !!(b)) -> !!(a))) -> (!!(b) -> ((a -> b) -> ((a -> !!(b)) -> !!(a)))))),
    (a -> b) -> (((a -> b) -> ((a -> !!(b)) -> !!(a))) -> (!!(b) -> ((a -> b) -> ((a -> !!(b)) -> !!(a))))),
    (a -> b) -> ((a -> !!(b)) -> !!(a)),
    ((a -> b) -> ((a -> !!(b)) -> !!(a))) -> ((a -> b) -> ((a -> b) -> ((a -> !!(b)) -> !!(a)))),
    (a -> b) -> ((a -> b) -> ((a -> !!(b)) -> !!(a))),
    ((a -> b) -> ((a -> b) -> ((a -> !!(b)) -> !!(a)))) -> (((a -> b) -> (((a -> b) -> ((a -> !!(b)) -> !!(a))) -> (!!(b) -> ((a -> b) -> ((a -> !!(b)) -> !!(a)))))) -> ((a -> b) -> (!!(b) -> ((a -> b) -> ((a -> !!(b)) -> !!(a)))))),
    ((a -> b) -> (((a -> b) -> ((a -> !!(b)) -> !!(a))) -> (!!(b) -> ((a -> b) -> ((a -> !!(b)) -> !!(a)))))) -> ((a -> b) -> (!!(b) -> ((a -> b) -> ((a -> !!(b)) -> !!(a))))),
    (a -> b) -> (!!(b) -> ((a -> b) -> ((a -> !!(b)) -> !!(a)))),
    ((a -> b) -> (!!(b) -> ((a -> b) -> ((a -> !!(b)) -> !!(a))))) -> (((a -> b) -> ((!!(b) -> ((a -> b) -> ((a -> !!(b)) -> !!(a)))) -> (!!(b) -> ((a -> !!(b)) -> !!(a))))) -> ((a -> b) -> (!!(b) -> ((a -> !!(b)) -> !!(a))))), ((a -> b) -> ((!!(b) -> ((a -> b) -> ((a -> !!(b)) -> !!(a)))) -> (!!(b) -> ((a -> !!(b)) -> !!(a))))) -> ((a -> b) -> (!!(b) -> ((a -> !!(b)) -> !!(a)))),
    (a -> b) -> (!!(b) -> ((a -> !!(b)) -> !!(a))),
    ((a -> b) -> (!!(b) -> ((a -> !!(b)) -> !!(a)))) -> (((a -> b) -> ((!!(b) -> ((a -> !!(b)) -> !!(a))) -> (!!(b) -> !!(a)))) -> ((a -> b) -> (!!(b) -> !!(a)))),
    ((a -> b) -> ((!!(b) -> ((a -> !!(b)) -> !!(a))) -> (!!(b) -> !!(a)))) -> ((a -> b) -> (!!(b) -> !!(a))),
    (a -> b) -> (!!(b) -> !!(a))
  )

  def tertiumNonDatur(e: Expr): Proof =
    List[Expr](e -> (e V !!(e))) ++
      contraposition(e, e V !!(e)) ++
      List[Expr](!!(e V !!(e)) -> !!(e)) ++
      List[Expr](!!(e) -> (e V !!(e))) ++
      contraposition(!!(e), e V !!(e)) ++
      List[Expr](!!(e V !!(e)) -> !!(!!(e))) ++
      List[Expr](
        (!!(e V !!(e)) -> !!(e)) -> ((!!(e V !!(e)) -> !!(!!(e))) -> !!(!!(e V !!(e)))),
        (!!(e V !!(e)) -> !!(!!(e))) -> !!(!!(e V !!(e))),
        !!(!!(e V !!(e))),
        !!(!!(e V !!(e))) -> (e V !!(e)),
        e V !!(e)
      )

  def disjunctionTT(a: Expr, b: Expr): Derivation = (List(a, b), List(a, a -> (a V b), a V b))
  def disjunctionTF(a: Expr, b: Expr): Derivation = (List(a, !!(b)), List(a, a -> (a V b), a V b))
  def disjunctionFT(a: Expr, b: Expr): Derivation = (List(!!(a), b), List(b, b -> (a V b), a V b))
  def disjunctionFF(a: Expr, b: Expr): Derivation = (List(!!(a), !!(b)), ???) // back contraposition (!!b -> !!a) -> (a -> b) needed :c
  def conjunctionTT(a: Expr, b: Expr): Derivation = (List(a, b), List(a, b, a -> (b -> (a & b)), b -> (a & b), a & b))
  def conjunctionTF(a: Expr, b: Expr): Derivation = (List(a, !!(b)), List(
    a & b -> b,
    !!(b),
    !!(b) -> (a & b -> !!(b)),
    (a & b) -> !!(b),
    ((a & b) -> b) -> (((a & b) -> !!(b)) -> !!(a & b)),
    ((a & b) -> !!(b)) -> !!(a & b),
    !!(a & b)
  ))
  def conjunctionFT(a: Expr, b: Expr): Derivation = (List(!!(a), b), List(
    a & b -> a,
    !!(a),
    !!(a) -> (a & b -> !!(a)),
    (a & b) -> !!(a),
    ((a & b) -> a) -> (((a & b) -> !!(a)) -> !!(a & b)),
    ((a & b) -> !!(a)) -> !!(a & b),
    !!(a & b)
  ))
  def conjunctionFF(a: Expr, b: Expr): Derivation = (List(!!(a), !!(b)), List(
    a & b -> a,
    !!(a),
    !!(a) -> (a & b -> !!(a)),
    (a & b) -> !!(a),
    ((a & b) -> a) -> (((a & b) -> !!(a)) -> !!(a & b)),
    ((a & b) -> !!(a)) -> !!(a & b),
    !!(a & b)
  ))
  def implicationTT(a: Expr, b: Expr): Derivation = (List(a, b), List(b -> (a -> b), b, a -> b))
  def implicationTF(a: Expr, b: Expr): Derivation = (List(a, !!(b)), List(
    a,
    !!(b),
    !!(b) -> a -> !!(b),
    a -> !!(b),
    (a -> !!(b)) -> ((a -> b) -> !!(a)), // FIXME TODO This must be proved (th. of deduction with context swaps maybe)
    (a -> b) -> !!(a),
    a -> (a -> b) -> a,
    (a -> b) -> a,
    ((a -> b) -> !!(a)) -> (((a -> b) -> a) -> !!(a -> b))
  ))
  def implicationFT(a: Expr, b: Expr): Derivation = (List(!!(a), b), List(b -> (a -> b), b, a -> b))
  def implicationFF(a: Expr, b: Expr): Derivation = (List(!!(a), !!(b)), ???)
  def negationT(a: Expr): Derivation = (List(a), List(
    a,
    a -> (!!(a) -> a),
    !!(a) -> a) ++
    ident(!!(a)) ++ List(
    (!!(a) -> a) -> ((!!(a) -> !!(a)) -> !!(!!(a))),
    (!!(a) -> !!(a)) -> !!(!!(a)),
    !!(!!(a))
  ))
  def negationF(a: Expr): Derivation = (List(!!(a)), List(!!(a)))
}
