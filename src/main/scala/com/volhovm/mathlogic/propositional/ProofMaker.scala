package com.volhovm.mathlogic.propositional

import Proofs._
import scala.language.implicitConversions

/**
 * @author volhovm
 *         Created on 10/15/14
 */

object ProofMaker {
  type Measure = List[(String, Boolean)]
  private type ProofTree = BTree[Either[Derivation, Measure]]

  private sealed trait BTree[A]
  private case class Node[A](lhs: BTree[A], rhs: BTree[A]) extends BTree[A] {
    override def toString: String = "Node:\n " + lhs.toString + "\n " + rhs.toString
  }
  private case class Leaf[A](a: A) extends BTree[A]


  private def excluding(x: Derivation, y: Derivation): Derivation = {
    if (x._1.tail != Nil && x._1.tail != y._1.tail)
      throw new IllegalArgumentException(IOUtil.header(x) ++ "\n" ++ IOUtil.header(y))
    else {
      val p = x._1.head
      val a = x._2.last
      (x._1.tail,
        deductionApply(x)._2 ++
          deductionApply(y)._2 ++
          tertiumNonDatur(p) ++ List[Expr](
          (p -> a) -> ((!!(p) -> a) -> ((p V !!(p)) -> a)),
          (!!(p) -> a) -> ((p V !!(p)) -> a),
          (p V !!(p)) -> a,
          a
        ))
    }
  }

  private def eval(e: Expr, vars: Measure): Boolean = e match {
    case a -> b => if (eval(a, vars) && !eval(b, vars)) false else true
    case a V b => eval(a, vars) | eval(b, vars)
    case a & b => eval(a, vars) & eval(b, vars)
    case !!(a) => !eval(a, vars)
    case Pred(a) => vars.find(c => c._1 == a).get._2
  }

  private def countVars(e: Expr, s: Set[String] = Set[String]()): Set[String] = e match {
    case a -> b => countVars(a, s) ++ countVars(b, s)
    case a V b => countVars(a, s) ++ countVars(b, s)
    case a & b => countVars(a, s) ++ countVars(b, s)
    case !!(a) => countVars(a, s)
    case Pred(a) => if (!s.contains(a)) s + a else s
  }

  private def ifNotVar[A](e: Expr, a: List[A]): List[A] = e match {
    case Pred(_) => Nil
    case _ => a
  }

  implicit private def measureToContext(measure: Measure): List[Expr] =
    measure.map(a => if (a._2) Pred(a._1) else !!(Pred(a._1)))

  private def foo(x: Expr, y: Expr, measure: Measure,
                  a: (Expr, Expr) => Derivation,
                  b: (Expr, Expr) => Derivation,
                  c: (Expr, Expr) => Derivation,
                  d: (Expr, Expr) => Derivation): Derivation = {
    val ex = eval(x, measure)
    val ey = eval(y, measure)
    mkD(ex match {
      case true => ey match {
        case true => (measure, ifNotVar(x, makeDerivation(measure, x)._2) ++
                        ifNotVar(y, makeDerivation(measure, y)._2) ++ a(x, y)._2)
        case false => (measure, ifNotVar(x, makeDerivation(measure, x)._2) ++
                         makeDerivation(measure, !!(y))._2 ++ b(x, y)._2)
      }
      case false => ey match {
        case true => (measure, makeDerivation(measure, !!(x))._2 ++
                        ifNotVar(y, makeDerivation(measure, y)._2) ++ c(x, y)._2)
        case false => (measure, makeDerivation(measure, !!(x))._2 ++
                         makeDerivation(measure, !!(y))._2 ++ d(x, y)._2)
      }
    })
  }

  private def makeDerivation(measure: Measure, e: Expr): Derivation = e match {
    case a -> b => foo(a, b, measure, implicationTT, implicationTF, implicationFT, implicationFF)
    case a V b => foo(a, b, measure, disjunctionTT, disjunctionTF, disjunctionFT, disjunctionFF)
    case a & b => foo(a, b, measure, conjunctionTT, conjunctionTF, conjunctionFT, conjunctionFF)
    case !!(a) => if (eval(a, measure))
      (measure, ifNotVar(a, makeDerivation(measure, a)._2) ++ negationT(a)._2)
                  else
      (measure, ifNotVar(a, makeDerivation(measure, a)._2) ++ negationF(a)._2)
    case Pred(_) => (List(e), List(e))
  }

  private def makeTree(
    e: Expr,
    list: List[String],
    out: Measure = List()
  ): ProofTree =
    list match {
      case x :: xs => Node(makeTree(e, xs, (x, true) :: out), makeTree(e, xs, (x, false) :: out))
      case Nil => if (eval(e, out)) Leaf(Left(makeDerivation(out.reverse, e)))
                  else Leaf(Right(out))
    }

  private def getDerivations(e: Expr): ProofTree =
    makeTree(e, countVars(e).toList)

  private def mergeAll(tree: ProofTree): Either[Derivation, Measure] =
    tree match {
      case Node(a, b) => mergeAll(a) match {
        case Left(x) => mergeAll(b) match {
          case Left(y) => Left(excluding(x, y))
          case r@Right(measure) => r
        }
        case r@Right(measure) => r
      }
      case Leaf(a) => a
    }

  def makeProof(e: Expr): Either[Proof, Measure] =
    mergeAll(getDerivations(e)) match {
      case Left(a) => Left(a._2)
      case Right(a) => Right(a)
    }
}
