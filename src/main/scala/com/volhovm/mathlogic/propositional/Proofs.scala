package com.volhovm.mathlogic.propositional

/**
 * @author volhovm
 *         Created on 10/6/14
 */

object Proofs {
  // Unannoteted: //
//////////////////////////////////////////////////////////
  def ident(e: Expr): Proof =
    List[Expr](
      e --> (e --> e),
      (e --> (e --> e)) --> ((e --> ((e --> e) --> e)) --> (e --> e)),
      e --> ((e --> e) --> e),
      (e --> ((e --> e) --> e)) --> (e --> e),
      e --> e
    )

  //TODO add more

  // Annoteted: //
//////////////////////////////////////////////////////////

  def identA(e: Expr, line: Int): AProof =
    List[(Expr, Constr)](
      (e --> (e --> e), Axiom(1)),
      ((e --> (e --> e)) --> ((e --> ((e --> e) --> e)) --> (e --> e)), Axiom(2)),
      (e --> ((e --> e) --> e), Axiom(1)),
      ((e --> ((e --> e) --> e)) --> (e --> e), ModusPonens(line + 1, line + 2)),
      (e --> e, ModusPonens(line + 2, line + 3))
    )

  def dedLink1A(e: Expr, cns: Constr, alpha: Expr, line: Int): AProof =
    List[(Expr, Constr)](
      (e, cns),
      (-->(e, --> (alpha, e)), Axiom(1)),
      (-->(alpha, e), ModusPonens(line + 1, line + 2))
    )

  def dedLink2A(e: Expr, alpha: Expr, expr1: Expr, n1: Int, expr2: Expr, n2: Int, line: Int): AProof =
    List[(Expr, Constr)](
      (-->(-->(alpha, expr1), -->(-->(alpha, -->(expr1, e)), -->(alpha, e))), Axiom(2)),
      (-->(-->(alpha, -->(expr1, e)), -->(alpha, e)), ModusPonens(n1, line)),
      (-->(alpha, e), ModusPonens(n2, line + 1))
    )
}
