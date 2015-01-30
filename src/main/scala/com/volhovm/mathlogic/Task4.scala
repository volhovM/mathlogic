package com.volhovm.mathlogic

import com.volhovm.mathlogic.propositional.IOUtil._
import com.volhovm.mathlogic.propositional._

/**
 * @author volhovm
 *         Created on 12/14/2015
 */

object Task4 {
  def main(args: Array[String]): Unit = {
    val fileName = if (args.length == 0 || args(0) == "") "fourth.in" else args(0)
    verdict(Annotator.annotateDerivation(getD(fileName))._2) match {
      case -1 => deductionApply(getD(fileName)) match {
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
      case n => Annotator.annotateDerivation(getD(fileName))._2(n - 1)._2 match {
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
        case x => println("Что-то поломалось в строчке " + n + " и выдает " + x.toString)
      }
    }
  }
}
