import AST._
import org.parboiled2.ParseError
import org.scalatest.{Matchers, path}
import org.scalatest.Inside._

import scala.util.{Failure, Success}

class LuceneQueryParserSpec extends path.FunSpec with Matchers {
  implicit class ScalatestCastable(it: AnyRef) {
    def as[T](implicit manifest: Manifest[T]) = {
      it shouldBe a [T]
      it.asInstanceOf[T]
    }
  }

  describe("LuceneQueryParser") {
    def parseQuery(inputString: String): Query = {
      val parser: LuceneQueryParser = new LuceneQueryParser(inputString)
      val result = parser.Input.run()
      result match {
        case Success(s) => s
        case Failure(f:ParseError) => fail(parser.formatError(f, showExpected = true, showPosition = true, showLine = false, showTraces = true), f)
        case Failure(f) => fail(s"Failed due to exception", f)
      }
    }

    describe("Parsing terms") {
      it("parses a query that contains only a single term") {
        val inputString = "go"

        val result = parseQuery(inputString)

        inside(result.expression) {
          case SingleTerm(t) => t should be ("go")
        }
      }

      it("parses a query that contains a phrase") {
        val inputString = "\"The Right Way\""

        val result = parseQuery(inputString)

        inside(result.expression) {
          case Phrase(t) => t should be ("The Right Way")
        }
      }
    }

    describe("Field Parsing") {
      it("parses a single field containing a single term") {
        val inputString = "text:go"

        val result = parseQuery(inputString)

        inside(result.expression) {
          case Field (name:String, t:SingleTerm) =>
            name should be ("text")
            t.text should be ("go")
        }
      }

      it("honors whitespace between the field name and term") {
        val inputString = "text : go"
        val result = parseQuery(inputString)

        inside(result.expression) {
          case Field (name:String, t:SingleTerm) =>
            name should be ("text")
            t.text should be ("go")
        }
      }

      it("parses a single field containing a phrase") {
        val inputString = "text:\"The Right Way\""

        val result = parseQuery(inputString)

        inside(result.expression) {
          case Field (name:String, t:Phrase) =>
            name should be ("text")
            t.text should be ("The Right Way")
        }
      }
    }

    describe("Operator Parsing") {
      it("parses a binary expression separated by an OR operator") {
        val inputString = "\"jakarta apache\" OR jakarta"

        val result = parseQuery(inputString)

        inside(result.expression) {
          case BinaryExpression(lhs, op, rhs) =>
            lhs.as[Phrase].text should be ("jakarta apache")
            op.value should be ("OR")
            rhs.as[SingleTerm].text should be ("jakarta")
        }
      }

      it("parses a binary expression separated by an AND operator") {
        val inputString = "\"jakarta apache\" AND jakarta"

        val result = parseQuery(inputString)

        inside(result.expression) {
          case BinaryExpression(lhs, op, rhs) =>
            lhs.as[Phrase].text should be ("jakarta apache")
            op.value should be ("AND")
            rhs.as[SingleTerm].text should be ("jakarta")
        }
      }

      it("parses a unary expression preceeded by a NOT operator") {
        val inputString = "NOT \"jakarta apache\""

        val result = parseQuery(inputString)

        inside(result.expression) {
          case UnaryExpression(op, exp) =>
            op.value should be ("NOT")
            exp.as[Phrase].text should be ("jakarta apache")
        }
      }

      it("parses a binary expression using a NOT operator") {
        val inputString = "\"jakarta apache\" NOT Lucene"

        val result = parseQuery(inputString)

        inside(result.expression) {
          case BinaryExpression(lhs, op, rhs) =>
            lhs.as[Phrase].text should be ("jakarta apache")
            op.value should be ("NOT")
            rhs.as[SingleTerm].text should be ("Lucene")
        }
      }
    }
  }
}
