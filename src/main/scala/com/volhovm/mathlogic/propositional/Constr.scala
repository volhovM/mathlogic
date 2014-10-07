package com.volhovm.mathlogic.propositional

/**
 * @author volhovm
 *         Created on 9/16/14
 */

sealed trait Constr
case class Fault() extends Constr
case class Axiom(num: Int) extends Constr
case class ModusPonens(lhs: Int, rhs: Int) extends Constr
case class Assumption() extends Constr