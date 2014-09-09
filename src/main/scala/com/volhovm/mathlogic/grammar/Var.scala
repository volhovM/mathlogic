package com.volhovm.mathlogic.grammar

import com.volhovm.mathlogic.grammar.Expr

/**
 * @author volhovm
 *         Created on 9/10/14
 */
case class Var[A](a: A) extends Expr[A]
