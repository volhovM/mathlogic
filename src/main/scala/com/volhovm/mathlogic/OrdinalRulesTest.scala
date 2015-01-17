package com.volhovm.mathlogic

import ordinals._
import ordinals.CNF._
import scala.util.Random

object OrdinalRulesTest extends App {
  def randCNF(seed: Double = 0.9): CNF =
    if (Random.nextDouble > 0.3)
      randAtom
    else randCList(seed)

  val randAtom: Atom = Atom(Random.nextInt(20))

  def randCList(seed: Double): CList =
    CList(
      (0 to (Random.nextInt((10 * seed).toInt + 1))).toList.map(
        _ => (randCNF(seed / 3))).sortWith(_ > _).distinct zip
        (1 to 50).toList.map(_ => BigInt(Random.nextInt(20) + 1)),
      randAtom)

  def rules(a: CNF, b: CNF, c: CNF, d: CNF) = List(
      ("a=a", (a, a))
        ,("b=b", (b, b))
        // plus
        ,("b<c => a + b < a + c", (zero, if ((b < c) & (a + b < a + c))
                                     zero else if (b >= c) zero else one))
        ,("a<w^b => a + w^b = w^b", (zero, if(a < CList(List((b, 1)), zero) &&
                                                a + CList(List((b, 1)), zero)
                                                == CList(List((b, 1)), zero))
                                      zero else if (a >= CList(List((b, 1)), zero)) zero else one))

        ,("a+(b+c) = (a+b)+c", (a + (b + c), (a + b) + c))
        ,("a+0=a", (a + zero, a))
        ,("0+a=a", (zero + a, a))
        // mul
        ,("a*(b*c) = (a*b)*c", (a * (b * c), (a * b) * c))
        ,("a*(b+1) = (a*b)+a", (a * (b + one), (a * b) + a))
        ,("b<c => a*b<a*c", (zero, if (b < c & (a * b < a * c))
                               zero else if (b >= c) zero else one))
        ,("b<c => a*b≤a*c", (zero, if (b < c & a * b <= a * c)
                               zero else if (b >= c) zero else one))
        ,("0*a=0", (zero * a, zero))
        ,("a*0=0", (a * zero, zero))
        // exp
        ,("a^0=1", (a ^ zero, one))
        ,("0^a=0", (zero ^ a, zero))
        ,("a^(b+1)=(a^b)*a", (a ^ (b + one), (a ^ b) * a))
        ,("a^b=(a^(b-1))*a if b < w", (a^b, if (b < po("w")) (a ^ (b - 1)) * a else a^b))
        ,("(a^b)^c = a^(b*c)", ((a ^ b) ^ c, a ^ (b * c)))

        // this rule is failing on a = 96, b = (w^96)*8 + 96, c = (w^96)*11 + 96
        // I tested it on normal calc too

         ,("(a^b)*(a^c)=(a^(b+c))", ((a ^ b) * (a ^ c), a ^ (b + c)))
    )

  def checkRules(list: List[(String,(CNF, CNF))]): Option[String] =
      list.foldLeft[Option[String]](None)((a: Option[String], b: (String, (CNF, CNF))) =>
        a match {
          case Some(errmsg) => Some(errmsg)
          case None         => if (b._2._1 == b._2._2) None
                               else Some(b._1 + ": \n" + b._2._1 + " !=\n" + b._2._2)
        })

  def runTests(tests: Int, hardness: Double) = {
    var error: Boolean = false
    for (i <- 0 to tests if !error) {
      val a = randCNF(i.toDouble / tests * hardness)
      val b = randCNF(i.toDouble / tests * hardness)
      val c = randCNF(i.toDouble / tests * hardness)
      val d = randCNF(i.toDouble / tests * hardness)
      checkRules(rules(a, b, c, d)) match {
        case None => println("Test #" + i + " OK")
        case Some(err) => { error = true; println("Test #" + i + " FAILED"
                                                  + ":\n" + err
                                                  + "\n a, b, c, d:\n" + a + "\n" + b + "\n"
                                                    + c + "\n" + d
                             ) }
      }
    }
  }

  runTests(100, 0.5)
}
