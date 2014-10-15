package com.volhovm.mathlogic.propositional

/**
 * @author volhovm
 *         Created on 10/6/14
 */

object Proofs {
  // Unannoteted: //
  def ident(a: Expr): Proof =
    List[Expr](
      a -> (a -> a),
      (a -> (a -> a)) -> ((a -> ((a -> a) -> a)) -> (a -> a)),
      a -> ((a -> a) -> a),
      (a -> ((a -> a) -> a)) -> (a -> a),
      a -> a
    )

  def deduction1(a: Expr, alpha: Expr): Proof =
    List[Expr](
      a,
      a -> (alpha -> a),
      alpha -> a
    )

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

  def disjunctionTT(a: Expr, b: Expr): Proof = ???
  def disjunctionTF(a: Expr, b: Expr): Proof = ???
  def disjunctionFT(a: Expr, b: Expr): Proof = ???
  def disjunctionFF(a: Expr, b: Expr): Proof = ???
  def conjunctionTT(a: Expr, b: Expr): Proof = ???
  def conjunctionTF(a: Expr, b: Expr): Proof = ???
  def conjunctionFT(a: Expr, b: Expr): Proof = ???
  def conjunctionFF(a: Expr, b: Expr): Proof = ???
  def implicationTT(a: Expr, b: Expr): Proof = ???
  def implicationTF(a: Expr, b: Expr): Proof = ???
  def implicationFT(a: Expr, b: Expr): Proof = ???
  def implicationFF(a: Expr, b: Expr): Proof = ???
  def negationT(a: Expr): Proof = ???
  def negationF(a: Expr): Proof = ???

  // Annoteted: //
  // TODO erase it
  def identA(e: Expr, line: Int): AProof =
    List[(Expr, Annotation)](
      (e -> (e -> e), Axiom(1)),
      ((e -> (e -> e)) -> ((e -> ((e -> e) -> e)) -> (e -> e)), Axiom(2)),
      (e -> ((e -> e) -> e), Axiom(1)),
      ((e -> ((e -> e) -> e)) -> (e -> e), ModusPonens(line + 1, line + 2)),
      (e -> e, ModusPonens(line + 2, line + 3))
    )

  def dedLink1A(e: Expr, cns: Annotation, alpha: Expr, line: Int): AProof =
    List[(Expr, Annotation)](
      (e, cns),
      (->(e, ->(alpha, e)), Axiom(1)),
      (->(alpha, e), ModusPonens(line + 1, line + 2))
    )

  def dedLink2A(e: Expr, alpha: Expr, expr1: Expr, n1: Int, expr2: Expr, n2: Int, line: Int): AProof =
    List[(Expr, Annotation)](
      (->(->(alpha, expr1), ->(->(alpha, ->(expr1, e)), ->(alpha, e))), Axiom(2)),
      (->(->(alpha, ->(expr1, e)), ->(alpha, e)), ModusPonens(n1, line)),
      (->(alpha, e), ModusPonens(n2, line + 1))
    )
}
