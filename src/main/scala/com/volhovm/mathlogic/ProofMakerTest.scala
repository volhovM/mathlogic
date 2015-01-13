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
  val e = Pred("e")
  val f = Pred("f")
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
      (a V b V c V d V e V f) -> (a & b & c & d & e & f),
      (c & !!(d & c) -> c -> b) V (a V !!(a)) // IT WORKS OH MY GOD
    )
  tautologies.foreach(x =>
    { println("Testing: " + x + "... ")
      makeProof(x) match {
        case Left(a) if verdict(Annotator.annotate(a)) == -1 =>
          println("[OK]"
                    // ATTENTION
                    // commenting next line gives performance boost
                    //+ "[" + shortenP(a).length + "]"
          )
        case Left(_) => println("[FAIL]")
        case Right(measure) => println("[FAIL] On set: " + measure.mkString(", "))
      }
    })
 }
