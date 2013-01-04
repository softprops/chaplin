package chaplin

import scala.util.parsing.combinator.RegexParsers

object Parse extends RegexParsers {

  override def skipWhitespace = false

  def ws: Parser[String] = """\s*""".r

  def any: Parser[String] = """.|(\r?\n)+""".r

  def id: Parser[String] = """[0-9A-Za-z-_.]+|[.]""".r

  def mustache: Parser[List[Chunk]] = 
    (tag | anythingBut(tag)).*

  def anythingBut[T](p: Parser[T]): Parser[Text] =
    (guard(p) ^^ { _ => Text("") }
    | rep1(not(p) ~> any) ^^ {
      t => Text(t.mkString(""))
    })

  def tag: Parser[Tag] =
    comment | section | variable

  def section: Parser[Section] =
    sectionOpen | sectionClose

  def sectionOpen: Parser[Section] =
    invertedOpen | standardOpen

  def standardOpen: Parser[SectionOpen] =
    "{{#" ~> ws ~> id  <~ ws <~ "}}" ^^ {
      case id => SectionOpen(id)
    }

  def invertedOpen: Parser[InvertedOpen] =
    "{{^" ~> ws ~> id <~ ws <~ "}}" ^^ {
      case id => InvertedOpen(id)
    }

  def comment: Parser[Comment] =
    "{{!" ~> ws ~> anythingBut("}}") <~ ws <~ "}}" ^^ {
      case Text(any) => Comment(any)
    }

  def sectionClose: Parser[SectionClose] =
    "{{/" ~> ws ~> id <~ ws <~ "}}" ^^ {
      case id => SectionClose(id)
    }

  def variable: Parser[Tag] =
    partial | unescVariable | escVariable

  def unescVariable: Parser[UnescapedVariable] =
    tripleStache | amperStache

  def tripleStache: Parser[UnescapedVariable] =
    "{{{" ~> ws ~> id <~ ws <~ "}}}" ^^ {
      case v => UnescapedVariable(v)
    }

  def amperStache: Parser[UnescapedVariable] =
    "{{&" ~> ws ~> id <~ ws <~ "}}" ^^ {
      case v => UnescapedVariable(v)
    }

  def escVariable: Parser[Variable] =
    "{{" ~> ws ~> id <~ ws <~ "}}" ^^ {
      case v => Variable(v)
    }

  def partial: Parser[Partial] =
    "{{>" ~> ws ~> id <~ ws <~ "}}" ^^ {
      case name => Partial(name)
    }

  def apply(in: String) = parseAll(mustache, in)
}
