//import com.volhovm.mathlogic.propositional.Annotator._
//import com.volhovm.mathlogic.propositional.Proofs._
//import com.volhovm.mathlogic.propositional.{verdict => verdictA, _}
//import org.scalatest.FlatSpec
//
///**
// * @author volhovm
// *         Created on 10/25/14
// */
//
//class ProofSpec extends FlatSpec {
//  // macros do not like implicit functions
//  def verdict(a: Proof) = verdictA(annotate(a))
//  "Ident" should "be correct" in {
//    assert(verdict(ident(Var('a'))) == -1)
//  }
//
//  "Contraposition" should "be correct" {
//    assert(verdict(contraposition(Var('a'), Var('b'))) == -1)
//  }
//
////  test("TertinumNonDatur is right") {
////    assert(verdict(tertiumNonDatur(Var('a'))) == -1)
////  }
////
////  test("Disjunction is right") {
////    assert(verdict(disjunctionFF(Var('a'), Var('b'))) == -1)
////    assert(verdict(disjunctionFT(Var('a'), Var('b'))) == -1)
////    assert(verdict(disjunctionTF(Var('a'), Var('b'))) == -1)
////    assert(verdict(disjunctionTT(Var('a'), Var('b'))) == -1)
////  }
////
////  test("Conjunction is right") {
////    assert(verdict(conjunctionFF(Var('a'), Var('b'))) == -1)
////    assert(verdict(conjunctionFT(Var('a'), Var('b'))) == -1)
////    assert(verdict(conjunctionTF(Var('a'), Var('b'))) == -1)
////    assert(verdict(conjunctionTT(Var('a'), Var('b'))) == -1)
////  }
////
////  test("Implication is right") {
////    assert(verdict(implicationFF(Var('a'), Var('b'))) == -1)
////    assert(verdict(implicationFT(Var('a'), Var('b'))) == -1)
////    assert(verdict(implicationTF(Var('a'), Var('b'))) == -1)
////    assert(verdict(implicationTT(Var('a'), Var('b'))) == -1)
////  }
////
////  test("Negation is right") {
////    assert(verdict(negationF(Var('a'))) == -1)
////    assert(verdict(negationT(Var('a'))) == -1)
////  }
//}
