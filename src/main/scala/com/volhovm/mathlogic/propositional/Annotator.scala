package com.volhovm.mathlogic.propositional

import scala.annotation.switch
import scala.collection._
import scala.collection.mutable.{HashMap, MultiMap, Set}

/**
 * @author volhovm
 *         Created on 9/10/14
 */

object Annotator {
  /**
   * Inner annotator state. It accumulates values from CMap, MPMap, Context to List[A]
   * @tparam A - type of List[A] you want to get.
   */
  private type State[A] = (CMap, MPMap, Context, List[A])

  /**
   * CMap contains map from all left expression parts to pair (it's annotation, line)
   */
  private type CMap = Map[Expr, (Annotation, Int)]

  /**
   * For all expressions of type a -> b MPMap contains key-value of b -> a
   */
  private type MPMap = HashMap[Expr, Set[(Expr, Int)]] with MultiMap[Expr, (Expr, Int)]

  /**
   * This method generates context from input to recognize Assumption
   * @param input - context (list of expressions)
   * @tparam A - the type of values inside list you want to get (see State[A])
   * @return - state with needed context
   */
  def contextState[A](input: Context) = (
    Map.empty[Expr, (Annotation, Int)],
    new HashMap[Expr, Set[(Expr, Int)]] with MultiMap[Expr, (Expr, Int)],
    input,
    List[A]())

  /**
   * Generates empty state when no Assumption() needed to recognize
   * @tparam A - see State[A]
   * @return - state with empty context
   */
  def emptyState[A]: State[A] = contextState[A](List[Expr]())

  /**
   * This method provides a possibility to operate with expressions and annotate a list of them
   * see annotate and annotateString methods as examples
   * @param exprs - the proof (see Proof)
   * @param state - the state, needed for evaluation
   * @param wrapper - the function that (for every expression) takes the Tuple3
   * and returns an A, accumulates it and returns a list of A
   * @param line - inner method param
   * @tparam A - the type parameter defining a list of what you want to get
   * @return - List[A]
   */
  def annotateGeneric[A](
      exprs: Proof,
      state: State[A],
      wrapper: (Expr, Annotation, Int) => A,
      line: Int = 0): List[A] =
    if (exprs.isEmpty) state._4.reverse
    else annotateGeneric(
      exprs.tail,
      wrap(
        state,
        line,
        exprs.head,
        getConstructionType(exprs.head, state),
        wrapper),
      wrapper,
      line + 1)

  /**
   * Returns usual annotation
   * (converts a list of expressions to list of pairs (expression, annotation))
   * @param exprs - expressions to annotate
   * @param state - state to provide if you have some context, default is emptyState
   * @return - annotated proof
   */
  def annotate(
      exprs: Proof,
      state: State[(Expr, Annotation)] = emptyState[(Expr, Annotation)]): AProof =
    annotateGeneric[(Expr, Annotation)](exprs, state, {(e, c, i) => (e, c)})

  /**
   * Returns annotated derivation
   * @param derivation - derivotion to annotate
   * @return - annotated derivation
   */
  def annotateDerivation(derivation: Derivation): ADerivation =
    (derivation._1, annotate(derivation._2, contextState(derivation._1)))

  /**
   * The method that matches expression and returns
   * it's annotation if succeeded or Fault() otherwise
   * @param x - expression to examine
   * @param state - current state
   * @tparam A - see State[A]
   * @return - annotation type for expression x
   */
  private def getConstructionType[A](x: Expr, state: State[A]): Annotation =
    x match {
      // Axioms
      case ((a -> b) -> ((c -> (d -> e)) -> (f -> g)))
          if a == c && b == d && e == g && a == f => Axiom(2)
      case ((a -> b) -> ((c -> d) -> ((e V f) -> g)))
          if a == e && b == d && c == f && d == g => Axiom(8)
      case ((a -> b) -> ((c -> !!(d)) -> !!(e))) if a == c && b == d && a == e => Axiom(9)
      case (a -> (b -> (c & d))) if a == c && b == d => Axiom(3)
      case ((a & b) -> c) if a == c => Axiom(4)
      case ((a & b) -> c) if b == c => Axiom(5)
        // TODO FREEDOM FOR SUBSTITUTION
//      case (@@(x, a) -> b) if { val t = diff(a, b);
//        (t._1 && t._2 == x.name | t._2 == "0") && freeForSubstitution(t._3, Term(t._2), a)
//      } => Axiom(11)
//      case (a -> ?(x, b)) if { val t = diff(b, a);
//        (t._1 && t._2 == x.name | t._2 == "0") && freeForSubstitution(t._3, Term(t._2), a)
//      } => Axiom(12)
      case (a -> (b V c)) if a == b => Axiom(6)
      case (a -> (b V c)) if a == c => Axiom(7)
      case (!!(!!(a)) -> b) if a == b => Axiom(10)
      case (a -> (b -> c)) if a == c => Axiom(1)
      case a if state._3.contains(a) => Assumption()
      case a if { val temp = isMP(a, state); temp._1 } => isMP(x, state)._2
//      case (?(a, b) -> c) if state._1.contains(b -> c) && !entersFree(c, a) =>
//        DerivationExists(state._1.get(b->c).get._2)
//      case (a -> @@(b, c)) if state._1.contains(a -> c) && !entersFree(a, b) =>
//        DerivationForall(state._1.get(a->c).get._2)
      case _ => Fault()
    }

  private def isMP[A](a: Expr, state: State[A]): (Boolean, Annotation) =
    if (state._2.contains(a)) state._2.get(a) match {
      case Some(set) if {
        set.filter(e => checkState(state._1.get(e._1 -> a))).nonEmpty } =>
        val (expr, newLine1) = set.filter(
            e => checkState(state._1.get(e._1 -> a))).reduceRight(
            (a, b) => if (checkState(state._1.get(a._1))) a else b)
        state._1.get(expr) match {
          case Some((Fault(), _)) => (false, Fault())
          case Some((_, newLine2)) => (true, ModusPonens(newLine2, newLine1))
          case _ => (false, Fault())
        }
      case _ => (false, Fault())
    } else (false, Fault())

  /**
   * Checks if this state.get contains Fault
   * Needed by getConstructionType
   */
  private def checkState(option: Option[(Annotation, Int)]): Boolean =
    option match {
      case None => false
      case Some((Fault(), _)) => false
      case _ => true
    }

  /**
   * This method updates state due to needed changes
   * @param state - state to modify
   * @param line - current line
   * @param e - current expression
   * @param annotation - annotation to x
   * @param wrapper - function from annotateGeneric
   * @tparam A - see State[A]
   * @return - new updated state
   */
  private def wrap[A](state: State[A],
                      line: Int,
                      e: Expr,
                      annotation: Annotation,
                      wrapper: (Expr, Annotation, Int) => A): State[A]
  = (state._1.+(e -> ((annotation, line))),
    e match {
      case y: -> =>
        state._2.addBinding(y.rhs, (y.lhs, line))
      case _ => state._2
    },
    state._3,
    wrapper(e, annotation, line) :: state._4)
}
