package com.volhovm.mathlogic.ordinals

import scala.language.implicitConversions
import com.volhovm.mathlogic.ordinals
import com.volhovm.mathlogic.ordinals._
import org.parboiled2._

class OrdinalParser(val input: ParserInput) extends Parser {
  type RO = Rule1[Ordinal]
  implicit private def wrpStr(s: String): Rule0 = rule {
      zeroOrMore(' ') ~ str(s) ~ zeroOrMore(' ')
    }

  private def leftAssoc[A](a: => Rule1[A], b: (A, A) => A, divider: String): Rule1[A]
    = rule { a ~ zeroOrMore(wrpStr(divider) ~ a ~> b) }

  def equalityOrdinals: Rule1[(Ordinal, Ordinal)] =
    rule {
      oneOrMore(ordExpr).separatedBy("=") ~> ((sq: Seq[Ordinal]) => (sq(0), sq(1)))
    }

  def ordExpr: RO  = leftAssoc(summable, ordinals.+, "+")
  private def summable: RO = leftAssoc(mullable, ordinals.*, "*")
  private def mullable: RO = rule {
      oneOrMore(unary).separatedBy("^") ~> ((_: Seq[Ordinal]).reduceRight(ordinals.^))
    }
  private def unary: RO = rule { omega | numeric | ("(" ~ ordExpr ~ ")") }
  private def omega: RO = rule { ch('w') ~> (() => W()) }
  private def numeric: RO =
    rule { capture(oneOrMore(anyOf("0123456789"))) ~>
      ((s: String) => Num(Integer.parseInt(s)))
    }
}
