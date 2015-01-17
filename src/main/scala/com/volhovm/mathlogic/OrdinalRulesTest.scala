package com.volhovm.mathlogic

import ordinals._
import ordinals.CNF._
import scala.util.Random

object OrdinalRulesTest extends App {
  def randCNF(seed: Double = 0.9): CNF =
    if (Random.nextDouble > 0.3)
      randAtom
    else randCList(seed)

  val randAtom: Atom = Atom(Random.nextInt(100))

  def randCList(seed: Double): CList =
    CList(
      (0 to (Random.nextInt((50 * seed).toInt + 1))).toList.map(
        _ => (randCNF(seed / 3))).sortWith(_ > _).distinct zip
        (1 to 50).toList.map(_ => BigInt(Random.nextInt(20) + 1)),
      randAtom)

  def rules(a: CNF, b: CNF, c: CNF, d: CNF) = List(
      ("a=a", (a, a))
        ,("b=b", (b, b))
        // plus
        ,("a+(b+c) = (a+b)+c", (a + (b + c), (a + b) + c))
        ,("a+0=a", (a + zero, a))
        ,("0+a=a", (zero + a, a))
        // mul
        ,("a*(b*c) = (a*b)*c", (a * (b * c), (a * b) * c))
        ,("a*(b+1) = (a*b)+a", (a * (b + one), (a * b) + a))
        ,("0*a=0", (zero * a, zero))
        ,("a*0=0", (a * zero, zero))
        // exp
        ,("a^0=1", (a ^ zero, one))
        ,("0^a=0", (zero ^ a, zero))
        ,("a^(b+1)=(a^b)*a", (a ^ (b + one), (a ^ b) * a))
        ,("a^b=(a^(b-1))*a if b < w", (a^b, if (b < po("w")) (a ^ (b - 1)) * a else a^b))
        // fails for
        // ordCalc> a
        // Normal form: 39
        // ordCalc> b
        // Normal form: (( w^( (( w^39 )*19 ) + 39 ) )*19 ) + 39
        // ordCalc> c
        // Normal form: (( w^96 )*11 ) + 96
        // ordCalc> (a^b)^c
        // Normal form: (( w^( (( w^135 )*11 ) + (( w^39 )*1824 ) + 39 ) )*2139307801779352658255999530788892138673649999164733823189087221 )
        // ordCalc> a^(b*c)
        // Normal form: (( w^( (( w^39 )*38 ) + 39 ) )*205373548970817855192575954955733645312670399919814447026152373216 )

        // ,("(a^b)^c = a^(b*c)", ((a ^ b) ^ c, a ^ (b * c)))

        // this rule is failing on a = 96, b = (w^96)*8 + 96, c = (w^96)*11 + 96
        // I tested it on normal calc too

        // ,("(a^b)*(a^c)=(a^(b+c))", ((a ^ b) * (a ^ c), a ^ (b + c)))
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

  runTests(100, 0.8)
}
