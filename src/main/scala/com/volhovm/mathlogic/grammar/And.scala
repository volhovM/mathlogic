package com.volhovm.mathlogic.grammar

import com.volhovm.mathlogic.grammar.Expr

/**
 * @author volhovm
 *         Created on 9/10/14
 */
case class And[A](a: Expr[A], b: Expr[A]) extends Expr[A] { override def toString = string2(" & ")(a, b)}
