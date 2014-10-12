package com.volhovm.mathlogic

import com.volhovm.mathlogic.propositional.IOUtil._
import com.volhovm.mathlogic.propositional._

/**
 * @author volhovm
 *         Created on 9/25/14
 */

// Task 2
object DeductionTransformer {
  def main(args: Array[String]) = printD(deductionApply(getD("deduction.in")))
}
