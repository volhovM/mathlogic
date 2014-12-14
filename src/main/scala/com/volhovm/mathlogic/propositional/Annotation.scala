package com.volhovm.mathlogic.propositional

/**
 * @author volhovm
 *         Created on 9/16/14
 */

sealed trait Annotation
case class Fault(faultType: FaultType) extends Annotation
case class Axiom(num: Int) extends Annotation
case class ModusPonens(lhs: Int, rhs: Int) extends Annotation
case class DerivationForall(other: Int) extends Annotation
case class DerivationExists(other: Int) extends Annotation
case class Assumption() extends Annotation

sealed trait FaultType
case class Common(descr: String) extends FaultType
case class NotFreeForSubst(theta: Expr, formula: Expr, variable: Term) extends FaultType
case class EntersFree(formula: Expr, variable: Term) extends FaultType
