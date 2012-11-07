package chaplin

trait Chunk
case class Text(text: String) extends Chunk

trait Tag extends Chunk

case class Variable(text: String) extends Tag
case class UnescapedVariable(text: String) extends Tag

trait Section extends Tag
case class SectionOpen(text: String) extends Section
case class InvertedOpen(text: String) extends Section
case class SectionClose(text: String) extends Section
case class Partial(name: String) extends Section
