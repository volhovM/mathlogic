package com.volhovm.mathlogic

import com.volhovm.mathlogic.ordinals._

/**
 * @author volhovm
 *         Created on 01/13/2015
 */

object Task8 {
  def main(args: Array[String]): Unit = {
    val fileName = if (args.length == 0 || args(0) == "") "task8.in" else args(0)
    scala.io.Source.fromFile("tests/" + fileName).getLines.toList.foreach(
      (input: String) => new OrdinalParser(input)
        .equalityOrdinals.run().get match {
        case (a, b) => println((if (oToC(a) == oToC(b)) "Равны" else "Не равны")
                                 + ": " + a + "=" + oToC(a) + ", " + b + "=" + oToC(b))
      })
  }
}
