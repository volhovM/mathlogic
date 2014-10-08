package com.volhovm.mathlogic

/**
 * @author volhovm
 *         Created on 10/7/14
 */

package object propositional {
  type Proof = List[Expr]
  type AProof = List[(Expr, Annotation)]
  type Context = List[Expr]
  type Derivation = (Context, Proof)
  type ADerivation = (Context, AProof)
  implicit def l(expr: Expr): Int = expr match {
    case Var(a) => 1
    case !!(a) => l(a) + 3
    case Var(_) --> Var(_) => 4
    case Var(_) &&& Var(_) => 3
    case Var(_) ||| Var(_) => 3
    case a --> b => l(a) + l(b) + 6
    case a &&& b => l(a) + l(b) + 5
    case a ||| b => l(a) + l(b) + 5
  }
}
