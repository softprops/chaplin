package chaplin

import scala.util.parsing.combinator.RegexParsers

object Parse extends RegexParsers {

  override def skipWhitespace = false

  def any: Parser[String] = """.|(\r?\n)+""".r

  def id: Parser[String] = """[0-9A-Za-z-_]+|[.]""".r

  def mustache: Parser[List[Chunk]] = 
    (tag | anythingBut(tag)).*

  def anythingBut[T](p: Parser[T]): Parser[Text] =
    (guard(p) ^^ { _ => Text("") }
    | rep1(not(p) ~> any) ^^ {
      t => Text(t.mkString(""))
    })

  def tag: Parser[Tag] =
    section | variable

  def section: Parser[Section] =
    sectionOpen | sectionClose | partial

  def sectionOpen: Parser[SectionOpen] =
    "{{#" ~> id <~ "}}" ^^ {
      case id => SectionOpen(id)
    }

  def sectionClose: Parser[SectionClose] =
    "{{/" ~> id <~ "}}" ^^ {
      case id => SectionClose(id)
    }

  def variable: Parser[Tag] =
    unescVariable | escVariable

  def unescVariable: Parser[UnescapedVariable] =
    "{{{" ~> id <~ "}}}" ^^ {
      case v => UnescapedVariable(v)
    }

  def escVariable: Parser[Variable] =
    "{{" ~> id <~ "}}" ^^ {
      case v => Variable(v)
    }

  def partial: Parser[Partial] =
    "{{>" ~> id <~ "}}" ^^ {
      case name => Partial(name)
    }

  def apply(in: String) = parseAll(mustache, in)
}
