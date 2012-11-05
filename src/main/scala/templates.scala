package chaplin

import java.io.{ PrintWriter => Writer }

trait Template {
  def apply(view: View, writer: Writer): Unit
}

object Templates {
  def mkTemplate(chunks: Seq[Chunk]): Template = new Template {
    def apply(view: View, writer: Writer) = {
      def applyView(view: View, chunks: Seq[Chunk], writer: Writer): Unit = {
        chunks match {
          case Nil => ()
          case head :: tail =>
            def applyNext = applyView(view, tail, writer)
            head match {
              case Text(txt) =>
                writer.write(txt)
                applyNext
              case Variable(name) =>
                view(name).map {
                  case StringVal(str) =>
                    writer.write(str)
                  case _ => ()
                }
                applyNext
              case UnescapedVariable(name) =>
                view(name).map {
                  case StringVal(str) =>
                    writer.write(str)
                  case _ => ()
                }
                applyNext
              case SectionOpen(name) =>
                val (untilClose, afterClose) = tail.toList.span(!_.isInstanceOf[SectionClose])
                view(name) match {                  
                  case None => applyView(view, afterClose, writer)
                  case Some(v:View) =>
                    applyView(v, untilClose, writer)
                  case Some(Falsy(bool)) =>
                    if (!bool) applyView(view,
                                         afterClose,
                                         writer)
                    else applyNext
                  case Some(IterableVal(it)) =>
                    it.foreach {
                      case v: View => applyView(v, untilClose, writer)
                      case StringVal(str) => writer.write(str)
                      case _ => ()
                    }
                    applyView(view, afterClose, writer)
                  case _ =>
                    applyView(view, afterClose, writer)
                }
              case SectionClose(_) => applyNext
            }
        }
      }
      applyView(view, chunks, writer)
    }
    override def toString = chunks.toString
  }
}
