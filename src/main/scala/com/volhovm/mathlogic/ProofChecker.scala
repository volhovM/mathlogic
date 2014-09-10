package com.volhovm.mathlogic

/**
 * @author volhovm
 *         Created on 9/10/14
 */

object ProofChecker {
  def main(args: Array[String]) {
    val parser = new ExpressionParser[String](identity)
    var maxlength = 30
    var lst: List[Expr[String]] = List.empty[Expr[String]]
    var curr: String = readLine()
    while (!curr.equals("quit")){
      if (curr.length > maxlength) maxlength = curr.length
      lst = parser.getExpression(curr) :: lst
      curr = readLine()
    }
    lst = lst.reverse
    var ctr = 0
    println(Verificator.verificate[String](lst)
      .toList
      .sortWith((a, b) => a._2._2 < b._2._2)
      .foreach(a => println(({ctr += 1; ctr} + ". %-" + (maxlength + 10) + "s%-20s").format(a._1, a._2._1))))
  }
}
