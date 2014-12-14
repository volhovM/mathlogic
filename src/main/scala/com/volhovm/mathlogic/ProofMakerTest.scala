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
  val a = Pred("a")
  val b = Pred("b")
  val c = Pred("c")
  val d = Pred("d")
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
      a,
      (a & !!(a)) -> b,
      (a -> b -> c) -> ((a & b) -> c),
      ((a & b) -> c) -> (a -> (b -> c)),
      (c -> a -> b) -> (c -> a) -> b,
      (c & !!(d & c) -> c -> b) V (a V !!(a)) // IT WORKS OH MY GOD
    )
  tautologies.foreach(x =>
    { println("Testing: " + x + "... ")
      makeProof(x) match {
        case Left(a) if verdict(Annotator.annotate(a)) == -1 =>
          println("[OK]" + "[" + shortenP(a).length + "]")
        case Left(_) => println("[FAIL]")
        case Right(measure) => println("[FAIL] On set: " + measure.mkString(", "))
      }
    })
 }
