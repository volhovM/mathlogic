package com.volhovm.mathlogic

import com.volhovm.mathlogic.propositional.IOUtil._
import com.volhovm.mathlogic.propositional._

/**
 * @author volhovm
 *         Created on 12/14/2015
 */

object Task4 {
  def main(args: Array[String]): Unit =
    verdict(Annotator.annotateDerivation(getD("fourth.in"))._2) match {
      case -1 => deductionApply(getD("fourth.in")) match {
        case Right(a) => printD(a)
        case Left((ann, e, v, top)) =>
          println("Вывод некорректен начиная с формулы номер "
                    + (getD("fourth.in")._2.indexOf(e) + 1).toString
                    + ": используется " + (ann match {
                                             case Axiom(n) => "схема аксиом"
                                             case DerivationForall(_) => "правило"
                                             case DerivationExists(_) => "правило"
                                             case _ => "черт пойми что"
                                           }) + " с квантором по переменной " + v.name
                    + ", входящей свободно в допущение " + top)
      }
      case n => Annotator.annotateDerivation(getD("fourth.in"))._2(n)._2 match {
        case Fault(reason) => reason match {
          case Common(text) =>
            println("Вывод некорректен начиная с формулы номер " + n + " " + text)
          case NotFreeForSubst(theta, formula, variable) =>
            println("Вывод некорректен начиная с формулы номер " + n + ": терм "
                      + theta + " не свободен для подстановки в формулу " + formula
                      + " вместо переменной " + variable.name)
          case EntersFree(formula, variable) =>
            println("Вывод некорректен начиная с формулы номер " + n + ": переменная"
                      + variable.name + " входит свободно в формулу " + formula)
        }
        case _ => println("Что-то поломалось, ой-ой-ой")
      }
    }
}
