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
      case ModusPonens(i, j) => shrt(proof.take(i + 1)) ++ shrt(proof.take(j + 1)) ++
                                List(proof.last._1)
      case Axiom(_) => List(proof.last._1)
      case Assumption() => List(proof.last._1)
      case DerivationForall(i) => shrt(proof.take(i)) ++ List(proof.last._1)
      case DerivationExists(i) => shrt(proof.take(i)) ++ List(proof.last._1)
      case Fault() => List()
    }

//  private def shrtA(proof: AProof): AProof =
//    proof.last._2 match {
//      case ModusPonens(i, j) =>
//        shrtA(proof.take(i + 1)) ++
//        shrtA(proof.take(j + 1)) ++
//          List(proof.last)
//      case Axiom(_) => List(proof.last)
//      case Assumption() => List(proof.last)
//      case Fault() => List()
//    }

  // if implicit, we have no second chance
  def shortenP(proof: Proof): Proof =
    shrt(Annotator.annotate(proof))
  def shortenD(derivation: Derivation): Derivation =
    (derivation._1, shrt(Annotator.annotateDerivation(derivation)._2))
  implicit def shortenAP(proof: AProof): AProof =
    Annotator.annotate(shrt(proof))
  implicit def shortenAD(derivation: ADerivation): ADerivation =
    Annotator.annotateDerivation((derivation._1, shrt(derivation._2)))

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
      case a & b           => subst(a, what, instead_of) & subst(b, what, instead_of)
      case a V b           => subst(a, what, instead_of) V subst(b, what, instead_of)
      case !!(a)           => !!(subst(a, what, instead_of))
      case @@(a, b)        => @@(a, subst(b, what, instead_of))
      case ?(a, b)         => ?(a, subst(b, what, instead_of))
      case Pred(a, l @ _*) => Pred(a, l.map(x => substT(x, what, instead_of)): _*)
      case t@Term(a, l @ _*) => subst(t, what, instead_of)
    }

  def substT(in: Term, what: Term, instead_of: Term): Term = in match {
      case a if a.args.length == 0 && instead_of.name == a.name => what
      case Term(a, l @ _*) => Term(a, l.map(x => substT(x, what, instead_of)): _*)
    }

  private def prod[A](a: (Boolean, String, A), b: (Boolean, String, A)) =
    (if (a._2 == "-1" | b._2 == "-1") false else
       if (a._1 == b._1 && a._1) a._2 == b._2 && a._3 == b._3
                                            else a._1 | b._1,
     if (a._2 == "-1" |
           b._2 == "-1" |
           a._1 == b._1 && a._1 && a._2 != b._2) "-1" else if (a._1) a._2 else b._2,
     if (a._1) a._3 else b._3)

  // checks if substituted is clear[x:=p]
  // if _1 is true then _3 is substituted instead of _2 var
  // if _1 is false then if _2 is "-1" then more than one var is subsituted
  //                     if _2 is variable then it has >1 different sibstitutions
  //                     if _2 is "0" exprs are equal
  //                     if _2 is "-2" exprs are not even similar
  def diff(clear: Expr, substituted: Expr): (Boolean, String, Expr) = clear match {
      case a -> b => substituted match {
        case c -> d => prod(diff(c, a), diff(d, b))
        case x => (false, "-2", x)
      }
      case a & b => substituted match {
        case c & d => prod(diff(a, c), diff(b, d))
        case x => (false, "-2", x)
      }
      case a V b => substituted match {
        case c V d => prod(diff(a, c), diff(b, d))
        case x => (false, "-2", x)
      }
      case !!(a) => substituted match {
        case !!(b) => diff(a, b)
        case x => (false, "-2", x)
      }
      case @@(a, b) => substituted match {
        case @@(c, d) => prod(diff(a, c), diff(b, d))
        case x => (false, "-2", x)
      }
      case ?(a, b) => substituted match {
        case ?(c, d) => prod(diff(a, c), diff(b, d))
        case x => (false, "-2", x)
      }
      case Pred(a, tail1 @ _*) => substituted match {
        case x@Pred(b, tail2 @ _*) if (a == b && tail2.length == tail1.length) =>
          if (tail1.isEmpty)
            (false, "0", x)
          else
            tail1.zip(tail2).map((a: (Expr, Expr)) => diff(a._1, a._2)).reduce(prod[Expr])
        case x => (false, "-2", x)
      }
        // variable
      case Term(a)
          if ((a.length == 2 && a(0).isLetter && a(1).isDigit) |
          (a.length == 1 && a(0).isLetter)) => substituted match {
            case c@Term(b) if a == b => (false, "0", c)
            case c : Term => (true, a, c)
            case x => (false, "-2", x)
      }
      case Term(a, tail1 @ _*) => substituted match {
        case x@Term(b, tail2 @ _*) if (a == b && tail1.length == tail2.length) =>
          // similar
          if (tail1.isEmpty)
            (false, "0", x)
          else
            tail1.zip(tail2).map((a: (Term, Term)) => diff(a._1, a._2)).reduce(prod[Expr])
        case x => (false, "-2", x)
      }
    }
  // for every var a -> list of quantors affecting it
  def getAffectedVars(e: Expr, quantors: Set[Term] = Set()): Map[Term, Set[Term]] =
      e match {
        case @@(a, b) => getAffectedVars(b, quantors + a)
        case ?(a, b)  => getAffectedVars(b, quantors + a)
        case v@Term(a)
            if ((a.length == 2 && a(0).isLetter && a(1).isDigit) |
                  (a.length == 1 && a(0).isLetter)) =>
          return Map(v -> quantors)
        case a -> b => getAffectedVars(a, quantors) ++ getAffectedVars(b, quantors)
        case a & b => getAffectedVars(a, quantors) ++ getAffectedVars(b, quantors)
        case a V b => getAffectedVars(a, quantors) ++ getAffectedVars(b, quantors)
        case !!(a) => getAffectedVars(a, quantors)
        case Pred(a) => Map()
        case Pred(a, l @ _*) => l.map(getAffectedVars(_, quantors)).reduce(_++_)
        case Term(a) => Map()
        case Term(a, l @ _*) => l.map(getAffectedVars(_, quantors)).reduce(_++_)
      }

  // alpha [v := theta]
  // theta was substituted instead of v in alpha, is it ok?
  def freeForSubstitution(theta: Expr, v: Term, alpha: Expr) : Boolean = {
    val a = getAffectedVars(theta)
    val b = getAffectedVars(alpha)
    b.get(v) match {
      case None => true
      case Some(set) => set.toList.map((x: Term) => a.get(x) match {
                                         case None => true
                                         case Some(y) => !y.isEmpty && y.contains(x)
                                       }).foldLeft(true)(_&&_)
    }
  }

  def entersFree(e: Expr, vr: Term) = getAffectedVars(e).get(vr) match {
      case Some(a) => !a.contains(vr)
      case None => false
    }
}
