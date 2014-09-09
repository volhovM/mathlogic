package com.volhovm.mathlogic.grammar

/**
 * @author volhovm
 *         Created on 9/4/14
 */

sealed trait Expr[A] {
  def string2(divider: String) = {(a: Any, b: Any) => "(" + a + divider + b + ")"}
}
