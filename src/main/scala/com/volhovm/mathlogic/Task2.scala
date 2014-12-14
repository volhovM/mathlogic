package com.volhovm.mathlogic

import com.volhovm.mathlogic.propositional.IOUtil._
import com.volhovm.mathlogic.propositional._

/**
 * @author volhovm
 *         Created on 9/25/14
 */

object Task2 {
  def main(args: Array[String]): Unit =
    printD(simpleDeductionApply(getD("second.in")))
}
