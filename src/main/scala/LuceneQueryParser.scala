import org.parboiled2._

object AST {
  case class Operator(value: String)

  sealed trait Expression
  sealed abstract class Term(text: String) extends Expression
  case class Field(name: String, term: Term) extends Expression
  case class SimpleExpression(term: Term) extends Expression
  case class UnaryExpression(operator: Operator, exp: Expression) extends Expression
  case class BinaryExpression(lhs: Expression, operator: Operator, rhs: Expression) extends Expression

  case class SingleTerm(text: String) extends Term(text)
  case class Phrase(text: String) extends Term(text)

  case class Query(expression: Expression)
}

class LuceneQueryParser(val input: ParserInput) extends Parser {
  import org.parboiled2.CharPredicate._

  def Input = rule { Expression ~ EOI ~> AST.Query }

  def Expression: Rule1[AST.Expression] = rule { UnaryExpression | BinaryExpression | FieldOrTerm }

  def FieldOrTerm: Rule1[AST.Expression] = rule { Field | Term }

  def Field: Rule1[AST.Field] = rule { capture(CharacterString) ~ MaybeWhitespace ~ ch(':') ~ MaybeWhitespace ~ Term ~> AST.Field }

  def Term: Rule1[AST.Term] = rule { Phrase | SingleTerm }

  def BinaryOperator: Rule1[AST.Operator] = rule { Whitespace ~ capture(ignoreCase("and") | ignoreCase("or") | ignoreCase("not")) ~ Whitespace ~> AST.Operator }

  def UnaryOperator: Rule1[AST.Operator] = rule { MaybeWhitespace ~ capture(ignoreCase("not")) ~ Whitespace ~> AST.Operator }

  def UnaryExpression: Rule1[AST.UnaryExpression] = rule { UnaryOperator ~ FieldOrTerm ~> AST.UnaryExpression }
  
  def BinaryExpression: Rule1[AST.BinaryExpression] = rule { FieldOrTerm ~ BinaryOperator ~ FieldOrTerm ~> AST.BinaryExpression }

  def Phrase: Rule1[AST.Phrase] = rule { ch('"') ~ capture(oneOrMore(CharacterString ~ MaybeWhitespace)) ~ ch('"') ~> (v => AST.Phrase(v)) }

  def SingleTerm: Rule1[AST.SingleTerm] = rule { optional('"') ~ capture(CharacterString) ~ optional('"') ~> (v => AST.SingleTerm(v)) }

  val WhiteSpaceChar = CharPredicate(" \n\r\t\f")
  def CharacterString = rule { oneOrMore(AlphaNum) }
  def Whitespace = rule { oneOrMore(WhiteSpaceChar) }
  def MaybeWhitespace = rule { zeroOrMore(WhiteSpaceChar) }
}
