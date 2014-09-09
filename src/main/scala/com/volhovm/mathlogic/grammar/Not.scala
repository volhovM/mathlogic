package com.volhovm.mathlogic.grammar

import com.volhovm.mathlogic.grammar.Expr

/**
 * @author volhovm
 *         Created on 9/10/14
 */
case class Not[A](a: Expr[A]) extends Expr[A] { override def toString: String = "!" + a }
