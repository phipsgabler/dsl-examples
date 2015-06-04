package dsl_examples

import scala.util.parsing.combinator.RegexParsers

object SExpParser {

  sealed trait SExpr
  case class Atom(value: String) extends SExpr
  protected case class ConsList(content: List[SExpr]) extends SExpr
  protected case class ReadMacro(symbol: String, quoted: SExpr) extends SExpr


  object Nil {
    def apply(): ConsList = ConsList(List())
    def unapply(expr: SExpr): Boolean = expr match {
      case ConsList(List()) => true
      case _ => false
    }
  }

  object Cons {
    def apply(x: SExpr, xs: ConsList): ConsList = ConsList(x::xs.content)
    def unapplySeq(expr: SExpr): Option[(SExpr, Seq[SExpr])] = expr match {
      case ConsList(head::tail) => Some(head, tail)
      case _ => None
    }
  }

  object Quote {
    def apply(e: SExpr): ReadMacro = ReadMacro(macroCharacter, e)
    def unapply(expr: SExpr): Option[SExpr] = expr match {
      case ReadMacro(macroCharacter, e) => Some(e)
      case _ => None
    }

    val macroCharacter: String = "'"
  }


  object SExpParser extends RegexParsers {
    override val skipWhitespace = false

    def parenthesized[T](p: Parser[T]) = "(" ~ whiteSpace.? ~> p <~ whiteSpace.? ~ ")"
    def identifier: Parser[String] = "[^()' ]+".r
    def readMacroIdentifier: Parser[String] = Quote.macroCharacter

    def atom: Parser[SExpr] = (identifier ^^ Atom) <~ whiteSpace.?
    def cons: Parser[SExpr] =  parenthesized(rep(sexpr) ^^ ConsList) <~ whiteSpace.?
    def readMacro: Parser[SExpr] = (readMacroIdentifier ~ sexpr) <~ whiteSpace.? ^^ {
      case s~e => ReadMacro(s, e)
    }
    def sexpr: Parser[SExpr] = whiteSpace.? ~> (readMacro | cons | atom)

    def apply(input : String) : Either[String, SExpr] = {
      parseAll(sexpr, input) match {
        case Success(result, _) => Right(result)
        case failure => Left(failure.toString)
      }
    }
  }

  def parse(s : String) : Either[String, SExpr] = SExpParser(s)
}


object SExpParserTest extends App {
  import SExpParser._


}
