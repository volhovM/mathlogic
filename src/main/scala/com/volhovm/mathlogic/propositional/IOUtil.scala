package com.volhovm.mathlogic.propositional

import com.volhovm.mathlogic.propositional.ExpressionParser

/**
 * @author volhovm
 *         Created on 10/7/14
 */

object IOUtil {
  def expressions(fileName: String): Proof =
    scala.io.Source.fromFile(fileName).getLines().toList.map((a: String) => new ExpressionParser(a).inputLine.run().get)

  def annotatedExpressions(fileName: String): AProof =
    Annotator.annotate(expressions(fileName))

  def stringAnnotatedExpressions(fileName: String): List[String] = {
    val proof = expressions(fileName)
    Annotator.annotateString(proof, proof.foldRight(0)({(e1, e2) => math.max(e1, e2)}))
  }
}
