package com.volhovm.mathlogic

/**
 * @author volhovm
 *         Created on 9/10/14
 */

object ProofChecker {
  def main(args: Array[String]) {
    var maxlength = 30
    var lst: List[Expr] = List.empty[Expr]
//    val iterator = scala.io.Source.fromFile("simpletest.in").getLines()
    val iterator = scala.io.Source.fromFile("maxtest.in").getLines()
    var curr = ""
    while (iterator.hasNext){
      curr = iterator.next()
      if (curr.length > maxlength) maxlength = curr.length
      lst = new ExpressionParser(curr).inputLine.run().get :: lst
    }
    TimeUtil.memTime("Reading + parsing")
    lst = lst.reverse
    TimeUtil.memTime("Reversing")
    val list = Verificator.verificate(lst, maxlength)
    TimeUtil.memTime("Verificating")
    list.foreach(println)
    TimeUtil.memTime("Reversing + writing")
    TimeUtil.dumpTime()
  }
}
