package com.volhovm.mathlogic

import com.volhovm.mathlogic.propositional.Annotator._
import com.volhovm.mathlogic.propositional.ProofMaker._
import com.volhovm.mathlogic.propositional._

/**
  * @author volhovm
  *         Created on 10/26/14
  */

// TODO arrange special dir fer tests
object ProofMakerTest extends App {
   val a = Var('a')
   val b = Var('b')
   val c = Var('c')
   val d = Var('d')
   val tautologies: List[Expr] = List(
     a -> a,
     b -> (a -> b),
     b V !!(b),
     !!(!!(b)) -> b,
     !!(a & !!(a)),
     a -> (b -> (a & b)),
     (a & b) -> a,
     (b & a) -> b,
     a -> (a V b),
     (c & !!(d & c) -> c -> b) V (a V !!(a)) // IT WORKS OH MY GOD
   )
   tautologies.foreach(x => { assert(verdict(annotate(makeProof(x))) == -1); println("Passed: " + x) })
 }
