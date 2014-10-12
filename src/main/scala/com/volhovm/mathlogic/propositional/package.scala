package com.volhovm.mathlogic

/**
 * @author volhovm
 *         Created on 10/7/14
 */

package object propositional {
  type Proof = List[Expr]
  type AProof = List[(Expr, Annotation)]
  type Context = List[Expr]
  type Derivation = (Context, Proof)
  type ADerivation = (Context, AProof)

  implicit def l(expr: Expr): Int = expr match {
    case Var(a) => 1
    case !!(a) => l(a) + 3
    case Var(_) --> Var(_) => 4
    case Var(_) &&& Var(_) => 3
    case Var(_) ||| Var(_) => 3
    case a --> b => l(a) + l(b) + 6
    case a &&& b => l(a) + l(b) + 5
    case a ||| b => l(a) + l(b) + 5
  }

  private def shrt(proof: AProof): Proof =
    proof.last._2 match {
      case ModusPonens(i, j) => shrt(proof.take(i + 1)) ++ shrt(proof.take(j + 1)) ++ List(proof.last._1)
      case Axiom(_) => List(proof.last._1)
      case Assumption() => List(proof.last._1)
      // let it fall if fault
    }

  def shortenP(proof: Proof): Proof = shrt(Annotator.annotate(proof))
  def shortenD(derivation: Derivation): Derivation = (derivation._1, shrt(Annotator.annotateDerivation(derivation)._2))
  def shortenAP(proof: AProof): Proof = shrt(proof)
  def shortenAD(derivation: ADerivation): Derivation = (derivation._1, shrt(derivation._2))

  def verdict(proof: AProof) = proof.takeWhile(_._2 match { case Fault() => false; case _ => true}) match {
    case a if a.length == proof.length => -1
    case a => a.length + 1
  }

  import Proofs._
  def deductionApply(d: Derivation): Derivation =
    shortenD(if (d._1.isEmpty) d else (d._1.tail, Annotator.annotateDerivation(d)._2.map {
      case (e, _) if e == d._1.head => ident(e)
      case (e, Axiom(n)) => deduction1(e, d._1.head)
      case (e, Assumption()) => deduction1(e, d._1.head)
      case (e, ModusPonens(n, m)) => deduction2(e, d._1.head, d._2(n), d._2(m)) // looks like I haven't messed up with indexes, but I'm not sure
    }.flatten))

  def deductionUnapply(d: Derivation): Derivation =
    d._2.last match {
      case a --> b => shortenD((a :: d._1, d._2.dropRight(1) :+ (a --> b) :+ a :+ b))
      case _ => d
    }
}
