package com.volhovm.mathlogic.propositional

/**
 * @author volhovm
 *         Created on 9/16/14
 */

sealed trait Annotation
case class Fault() extends Annotation
case class Axiom(num: Int) extends Annotation
case class ModusPonens(lhs: Int, rhs: Int) extends Annotation
case class Assumption() extends Annotation