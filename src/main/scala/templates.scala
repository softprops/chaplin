package chaplin

import java.io.{ PrintWriter => Writer }

trait Template {
  def apply(view: View, writer: Writer): Unit
}

object Templates {
  val Self = "."
  def esc(in: String) = in // todo
  def mkTemplate(chunks: Seq[Chunk]): Template = new Template {
    def apply(view: View, writer: Writer) = {
      def applyValue(value: Value, chunks: Seq[Chunk], writer: Writer): Unit = {
        chunks match {
          case Nil => ()
          case head :: tail =>
            def applyNext(nextWriter: Writer) = applyValue(value, tail, nextWriter)
            head match {
              case Text(txt) =>
                writer.write(txt)
                applyNext(writer)
              case Variable(name) =>
                value match {
                  case view:View => view(name).map {
                    case StringVal(str) =>
                      writer.write(esc(str))
                    case _ => ()
                  }
                  case StringVal(str) if (Self == name) =>
                    writer.write(esc(str))
                  case _ => ()
                }
                applyNext(writer)
              case UnescapedVariable(name) =>
                value match {
                  case view: View => view(name).map {
                    case StringVal(str) =>
                      writer.write(str)
                    case _ => ()
                  }
                  case StringVal(str) if (Self == name) =>
                    writer.write(str)
                  case _ => ()
                }
                applyNext(writer)
              case InvertedOpen(name) =>
                val (untilClose, afterClose) = tail.toList.span(!_.isInstanceOf[SectionClose])
                value match {
                  case view: View => view(name) match {
                    case None =>
                      applyNext(writer)
                    case Some(v: View) =>
                      applyValue(value, afterClose, writer)
                    case Some(Falsy(bool)) =>
                      if (bool) applyValue(value, afterClose, writer)
                      else applyNext(writer)
                    case Some(IterableVal(it)) =>
                      if (it.isEmpty) applyNext(writer)
                      else applyValue(value, afterClose, writer)
                    case _ => applyNext(writer)
                  }
                  case Falsy(bool) =>
                    if (bool) applyValue(value, afterClose, writer)
                    else applyNext(writer)
                  case IterableVal(it) =>
                     if (it.isEmpty) applyNext(writer)
                     else applyValue(value, afterClose, writer)
                  case _ => ()
                }
              case SectionOpen(name) =>
                val (untilClose, afterClose) = tail.toList.span(!_.isInstanceOf[SectionClose])
                value match {
                  case view: View => view(name) match {
                    case None =>
                      applyValue(value, afterClose, writer)
                    case Some(v: View) =>
                      applyValue(v, untilClose, writer)
                      applyValue(view, afterClose, writer)
                    case Some(Falsy(bool)) =>
                      if (!bool) applyValue(view,
                                            afterClose,
                                            writer)
                      else applyNext(writer)
                    case Some(IterableVal(it)) =>
                      it.foreach(applyValue(_, untilClose, writer))
                      applyValue(view, afterClose, writer)
                    case _ =>
                      applyValue(view, afterClose, writer)
                  }
                  case _ => ()
                }
              case SectionClose(_) => applyNext(writer)
            }
        }
      }
      applyValue(view, chunks, writer)
    }
    override def toString = chunks.toString
  }
}
