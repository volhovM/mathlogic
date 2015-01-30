package com.volhovm.mathlogic

import com.volhovm.mathlogic.ordinals._

/**
 * @author volhovm
 *         Created on 01/13/2015
 */

object Task5 {
  def main(args: Array[String]): Unit =
      scala.io.Source.fromFile("tests/fifth.in").getLines.toList.foreach(
        (input: String) => new OrdinalParser(input)
          .equalityOrdinals.run().get match {
          case (a, b) => println((if (oToC(a) == oToC(b)) "Равны" else "Не равны")
                                   + ": " + a + "=" + oToC(a) + ", " + b + "=" + oToC(b))
        })
}
