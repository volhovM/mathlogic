package com.volhovm.mathlogic

import com.volhovm.mathlogic.propositional.IOUtil._

/**
 * @author volhovm
 *         Created on 9/10/14
 */

object ProofChecker {
  def main(args: Array[String]): Unit = stringAnnotatedExpressions("simpletest.in").foreach(println)
}
