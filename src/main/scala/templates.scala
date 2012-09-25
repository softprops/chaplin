package chaplin

import java.io.Writer

trait Template {
  def apply(view: View, writer: Writer)
}

object Templates {
  def mkTemplate(chunks: Seq[Chunk]): Template = new Template {
    def apply(view: View, writer: Writer) =
      chunks.foreach {
        case Text(txt) => writer.write(txt)
        case Variable(name) => println(view.resolve(name))
        case UnescapedVariable(name) => println(view.resolve(name))
        case SectionOpen(name) => println(view.resolve(name))
        case SectionClose(name) => println(view.resolve(name))
        case Partial(name) => println(view.resolve(name))
      }
    override def toString = chunks.toString
  }
}
