package com.volhovm.mathlogic

import java.io.PrintWriter

import propositional._
import IOUtil._

/**
 * @author volhovm
 *         Created on 9/10/14
 */

// Task 1
object ProofChecker {
  def main(args: Array[String]): Unit = verdict(getAP("maxtest.in")) match {
    case -1 => print("Доказательство корректно.")
    case n  => print("Доказательство некорректно начиная с " + n + " высказывания.")
  }
}
