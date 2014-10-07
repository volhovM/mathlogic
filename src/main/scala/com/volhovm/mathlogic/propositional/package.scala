package com.volhovm.mathlogic

import scala.collection.Map
import scala.collection.mutable.{MultiMap, Set, HashMap}

/**
 * @author volhovm
 *         Created on 10/7/14
 */

package object propositional {
  type Proof = List[Expr]
  type AProof = List[(Expr, Constr)]
  type Context = List[Expr]
  implicit def l(expr: Expr): Int = expr match {
    case Var(a) => 1
    case !!(a) => l(a) + 3
    case a --> b => l(a) + l(b) + 6
    case a &&& b => l(a) + l(b) + 5
    case a ||| b => l(a) + l(b) + 5
  }
}
