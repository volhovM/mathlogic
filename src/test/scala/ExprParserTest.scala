import org.specs2.mutable.Specification

/**
 * @author volhovm
 *         Created on 9/9/14
 */

class ExprParserTest extends Specification {
  val parser = new ExpressionParser[String](identity)
  "Simple char parser" should {
    "support equality" in {
      parser.getExpression("a") must_== Var("a")
    }
    "support right operator priority" in {
      parser.getExpression("a | B | c ") must_== Or(Or(Var("a"), Var("B")), Var("c"))
      parser.getExpression("a & B | c ") must_== Or(And(Var("a"), Var("B")), Var("c"))
      parser.getExpression("a -> B & c ") must_== Implication(Var("a"), And(Var("B"), Var("c")))
      parser.getExpression("!!!!a") must_== Not(Not(Not(Not(Var("a")))))
    }
    "have no problems with parenthesis" in {
      parser.getExpression("((((((a))))))") must_== parser.getExpression("a")
      parser.getExpression("a -> b & a | c -> !x -> !x|x -> a") must_== parser.getExpression("(a -> (((b & a) | c) -> (!x -> ((!x|x) -> a))))")
    }
  }
}
