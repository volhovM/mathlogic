package com.volhovm.mathlogic

import com.volhovm.mathlogic.propositional._
import Annotator._
import ProofMaker._
import IOUtil._

/**
  * @author volhovm
  *         Created on 10/26/14
  */

// TODO arrange special dir fer tests
object Task3 {
  def main(args: Array[String]): Unit = makeProof(getP("third.in")(0)) match {
      case Left(a) if verdict(Annotator.annotate(a)) == -1 => printP(shortenP(a))
      case Left(_) => println("We've made up a proof, but it isn't right. Eh? U baka?")
      case Right(measure) =>
        println("Высказывание ложно при " +
                  measure.map(
                    (a) => a._1.toString + "=" + (if (a._2) "И" else "Л")
                  ).mkString(", "))
    }
}
