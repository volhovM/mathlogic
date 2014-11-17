package com.volhovm.mathlogic

import scala.language.implicitConversions

/**
 * @author volhovm
 *         Created on 10/7/14
 */

package object propositional {
  def hole = ???

  type Proof = List[Expr]
  type AProof = List[(Expr, Annotation)]
  type Context = List[Expr]
  type Derivation = (Context, Proof)
  type ADerivation = (Context, AProof)

  // FIXME pred, term do not take into account params
  implicit def l(expr: Expr): Int = expr match {
      case Pred(a, list @ _*) => a.length + list.length + 2 + list.foldLeft(0)((a, b) => a + l(b))
      case Term(a, list @ _*) => a.length + list.length + 2 + list.foldLeft(0)((a, b) => a + l(b))
      case @@(s, a) => l(a) + 1
      case ?(s, a) => l(a) + 1
      case !!(a) => l(a) + 1
      case a -> b => l(a) + l(b) + 4
      case a & b => l(a) + l(b) + 3
      case a V b => l(a) + l(b) + 3
  }

  private def shrt(proof: AProof): Proof =
    proof.last._2 match {
      case ModusPonens(i, j) => shrt(proof.take(i + 1)) ++
                                shrt(proof.take(j + 1)) ++
                                List(proof.last._1)
      case Axiom(_) => List(proof.last._1)
      case Assumption() => List(proof.last._1)
      case Fault() => List()
    }

  // if implicit, we have no second chance
  implicit def shortenP(proof: Proof): Proof =
    shrt(Annotator.annotate(proof))
  implicit def shortenD(derivation: Derivation): Derivation =
    (derivation._1, shrt(Annotator.annotateDerivation(derivation)._2))
  implicit def shortenAP(proof: AProof): Proof =
    shrt(proof)
  implicit def shortenAD(derivation: ADerivation): Derivation =
    (derivation._1, shrt(derivation._2))

  def verdict(proof: AProof) =
    proof.takeWhile(_._2 match { case Fault() => false; case _ => true}) match {
      case a if a.length == proof.length => -1
      case a => a.length + 1
  }

  import Proofs._

  def mkD(d: Derivation): Derivation = (d._1.reverse.distinct, d._2)

  def deductionApply(d: Derivation): Derivation =
    shortenD(if (d._1.isEmpty) d
    else (d._1.tail, Annotator.annotateDerivation(d)._2.map {
      case (e, _) if e == d._1.head => ident(e)
      case (e, Axiom(n)) => deduction1(e, d._1.head)._2
      case (e, Assumption()) => deduction1(e, d._1.head)._2
          // looks like I haven't messed up with indexes, but I'm not sure
      case (e, ModusPonens(n, m)) =>
              deduction2(e, d._1.head, d._2(n), d._2(m))
    }.flatten))

  def deductionUnapply(d: Derivation): Derivation =
    d._2.last match {
      case a -> b => shortenD((a :: d._1, d._2.dropRight(1) :+ (a -> b) :+ a :+ b))
      case _ => d
    }

  def subst(in: Expr, what: Term, instead_of: Term): Expr = in match {
      case a -> b          => subst(a, what, instead_of) -> subst(b, what, instead_of)
      case a & b          => subst(a, what, instead_of) & subst(b, what, instead_of)
      case a V b          => subst(a, what, instead_of) V subst(b, what, instead_of)
      case !!(a)           => !!(subst(a, what, instead_of))
      case @@(a, b)        => @@(a, subst(b, what, instead_of))
      case ?(a, b)         => ?(a, subst(b, what, instead_of))
      case Pred(a, l @ _*) => Pred(a, l.map(x => substT(x, what, instead_of)): _*)
    }

  def substT(in: Term, what: Term, instead_of: Term): Term = in match {
      case a if a.args.length == 0 && instead_of.name == a.name => what
      case Term(a, l @ _*) => Term(a, l.map(x => substT(x, what, instead_of)): _*)
    }


  def prod[A](a: (Boolean, A), b: (Boolean, A)) = ((a._1 == b._1) && (a._2 == b._2), a._2)
  def diff(a: Expr, b: Expr) = ???
}
