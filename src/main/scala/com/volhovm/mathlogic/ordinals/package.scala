package com.volhovm.mathlogic
import scala.language.implicitConversions

package object ordinals {
  //parse_ordinal
  implicit def po(str: String) = new OrdinalParser(str).ordExpr.run().get
  def w = po("w")
  val zero = Atom(0)
  implicit def oToC(o: Ordinal): CNF =
    o match
    {
      case W() => CList(List((Atom(1), 1)), zero)
      case Num(a) => Atom(a)
      case +(a, b) => oToC(a) + oToC(b)
      case *(a, b) => oToC(a) * oToC(b)
      case ^(a, b) => oToC(a) ^ oToC(b)
    }
  implicit def intToAtom(i: Int): Atom = oToC(po(i.toString)).asInstanceOf[Atom]
}
