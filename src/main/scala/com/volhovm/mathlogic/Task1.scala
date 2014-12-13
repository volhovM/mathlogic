package com.volhovm.mathlogic

import propositional._
import IOUtil._

/**
 * @author volhovm
 *         Created on 9/10/14
 */

object Task1 {
  def main(args: Array[String]): Unit = verdict(getAP("first.in")) match {
    case -1 => print("Доказательство корректно.")
    case n  => print("Доказательство некорректно начиная с " + n + " высказывания.")
  }
}
