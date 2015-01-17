package com.volhovm.mathlogic

import scala.language.implicitConversions
import scala.language.reflectiveCalls
import scala.math.BigInt

package object ordinals {
  //parse_ordinal
  implicit def po(str: String) = oToC(new OrdinalParser(str).ordExpr.run().get)
  implicit def bigintpow(b: BigInt) = new {
      def powB(that: BigInt): BigInt =
        if (that.isValidInt)
          b pow that.intValue()
        else
          throw new Exception("exp is too big")
    }
  def w = po("w")
  val zero = Atom(BigInt(0))
  val one = Atom(BigInt(1))
  implicit def intToBigint(i: Int): BigInt = BigInt(i)
  implicit def oToC(o: Ordinal): CNF =
    o match
    {
      case W() => CList(List((one, 1)), zero)
      case Num(a) => Atom(BigInt(a))
      case +(a, b) => oToC(a) + oToC(b)
      case *(a, b) => oToC(a) * oToC(b)
      case ^(a, b) => oToC(a) ^ oToC(b)
    }
  implicit def intToAtom(i: Int): Atom = po(i.toString).asInstanceOf[Atom]
}
