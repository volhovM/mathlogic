package com.volhovm.mathlogic.propositional

import Proofs._

/**
 * @author volhovm
 *         Created on 10/15/14
 */

object ProofMaker {
  sealed trait BTree[A]
  case class Node[A](lhs: BTree[A], rhs: BTree[A]) extends BTree[A]
  case class Leaf[A](a: A) extends BTree[A]

  def excluding(x: Derivation, y: Derivation): Derivation = {
    if (x._1.tail != y._1.tail) throw new IllegalArgumentException
    else {
      val p = x._1.head
      val a = x._2.head
      (x._1.tail, deductionApply(x)._2 ++ deductionApply(y)._2 ++ tertiumNonDatur(p) ++ List[Expr](
        (p -> a) -> ((!!(p) -> a) -> ((p V !!(p)) -> a)),
        (!!(p) -> a) -> ((p V !!(p)) -> a),
        (p V !!(p)) -> a,
        a
      ))
    }
  }

  private def eval(e: Expr, vars: List[(Char, Boolean)]): Boolean = e match {
    case a -> b => if (eval(a, vars) && !eval(b, vars)) true else false
    case a V b  => eval(a, vars) | eval(b, vars)
    case a & b  => eval(a, vars) & eval(b, vars)
    case !!(a)  => !eval(a, vars)
    case Var(a) => vars.find(c => c._1 == a).get._2
  }

  private def countVars(e: Expr, s: Set[Char] = Set[Char]()): Set[Char] = e match {
    case a -> b => countVars(a, s) ++ countVars(b, s)
    case a V b  => countVars(a, s) ++ countVars(b, s)
    case a & b  => countVars(a, s) ++ countVars(b, s)
    case !!(a)  => countVars(a, s)
    case Var(a) => if (!s.contains(a)) s + a else s
  }

  private def foo(x: Expr, y: Expr, measure: List[(Char, Boolean)],
                  a: (Expr, Expr) => Proof,
                  b: (Expr, Expr) => Proof,
                  c: (Expr, Expr) => Proof,
                  d: (Expr, Expr) => Proof): Derivation = {
    val ex = eval(x, measure)
    val ey = eval(y, measure)
    ex match {
      case true   => ey match {
        case true   => (List[Expr](x, y)        , makeDerivation(measure, x)._2 ++ makeDerivation(measure, y)._2 ++ a(x, y))
        case false  => (List[Expr](x, !!(y))    , makeDerivation(measure, x)._2 ++ makeDerivation(measure, y)._2 ++ b(x, y))
      }
      case false  => ey match {
        case true   => (List[Expr](!!(x), y)    , makeDerivation(measure, x)._2 ++ makeDerivation(measure, y)._2 ++ c(x, y))
        case false  => (List[Expr](!!(x), !!(y)), makeDerivation(measure, x)._2 ++ makeDerivation(measure, y)._2 ++ d(x, y))
      }
    }
  }

  private def makeDerivation(measure: List[(Char, Boolean)], e: Expr): Derivation = e match {
    case a -> b => foo(a, b, measure, implicationTT, implicationTF, implicationFT, implicationFF)
    case a V b => foo(a, b, measure, disjunctionTT, disjunctionTF, disjunctionFT, disjunctionFF)
    case a & b => foo(a, b, measure, conjunctionTT, conjunctionTF, conjunctionFT, conjunctionFF)
    case !!(a)  =>  if (eval(a, measure)) (List(a), makeDerivation(measure, a)._2 ++ negationT(a))
                    else (List(!!(a)), makeDerivation(measure, a)._2 ++ negationF(a))
    case Var(_) => (List(e), List(e))
  }

  private def makeTree(e: Expr, list: List[Char], out: List[(Char, Boolean)] = List[(Char, Boolean)]()): BTree[Derivation] =
    list match {
      case x :: xs  => Node(makeTree (e, xs, (x, false) :: out), makeTree (e, xs, (x, true) :: out) )
      case Nil      => Leaf(makeDerivation(out, e))
    }

  private def getDerivations(e: Expr): BTree[Derivation] = makeTree(e, countVars(e).toList)

  private def mergeAll(tree: BTree[Derivation]): Derivation = tree match {
    case Node(Leaf(a), Leaf(b)) => excluding(a, b)
    case Node(a, b)             => excluding(mergeAll(a), mergeAll(b))
    case Leaf(a)                => a
  }

  def makeProof(e: Expr): Proof = mergeAll(getDerivations(e))._2
}
