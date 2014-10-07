package com.volhovm.mathlogic

import com.volhovm.mathlogic.propositional.IOUtil

/**
 * @author volhovm
 *         Created on 9/10/14
 */

object ProofChecker {
  def main(args: Array[String]): Unit =
    (Stream.from(1) zip IOUtil.stringAnnotatedExpressions("maxtest.in")).foreach(println)
}
