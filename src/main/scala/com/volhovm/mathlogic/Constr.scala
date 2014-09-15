package com.volhovm.mathlogic

/**
 * @author volhovm
 *         Created on 9/16/14
 */

sealed trait Constr
case class Fault() extends Constr
case class Axiom(num: Int) extends Constr
case class ModusPonens(num1: Int, num2: Int) extends Constr
// TODO Deduction