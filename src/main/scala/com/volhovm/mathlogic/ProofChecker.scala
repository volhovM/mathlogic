package com.volhovm.mathlogic

/**
 * @author volhovm
 *         Created on 9/10/14
 */

object ProofChecker {
  def main(args: Array[String]) {
    val time0 = System.currentTimeMillis()
    var maxlength = 30
    var lst: List[Expr] = List.empty[Expr]
    val iterator = scala.io.Source.fromFile("maxtest1.in").getLines()
    var curr = ""
    while (iterator.hasNext){
      curr = iterator.next()
      if (curr.length > maxlength) maxlength = curr.length
      lst = new ExpressionParser(curr).inputLine.run().get :: lst
    }
    lst = lst.reverse
    val time1 = System.currentTimeMillis()
    var ctr = 0
    println(Verificator.verificate(lst)
      .toList
      .sortWith((a, b) => a._2._2 < b._2._2)
      .foreach(a => println(({ctr += 1; ctr} + ". %-" + (maxlength + 10) + "s%-20s").format(a._1, a._2._1))))
    print((time1 - time0) + " " + (System.currentTimeMillis() - time1))
  }
}
