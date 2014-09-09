package com.volhovm.mathlogic

/**
 * @author volhovm
 *         Created on 9/10/14
 */
object MathProver {
  def main(args: Array[String]) {
    val parser = new ExpressionParser[String](identity)
    print(parser.getExpression(readLine()))
  }
}
