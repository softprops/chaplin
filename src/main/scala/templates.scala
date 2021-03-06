package chaplin

import java.io.Writer

trait Template {
  def apply(view: View, writer: Writer): Writer
}

object Templates {
  val Self = "."
  def esc(in: String) = Esc(in)
  def mkTemplate(chunks: Seq[Chunk], partials: Map[String, Seq[Chunk]] = Map.empty[String, Seq[Chunk]]): Template = new Template {
    def apply(view: View, writer: Writer): Writer = {
      def applyValue(value: Value, chunks: Seq[Chunk], writer: Writer, standalone: Boolean = false): Writer = {
        chunks match {
          case Nil => writer
          case head :: tail =>
            def applyNext(nextWriter: Writer, standalone: Boolean = false) =
              applyValue(value, tail, nextWriter, standalone)
            head match {
              case Partial(name) =>
                val pchunks = partials.get(name).getOrElse(Nil)
                applyValue(value, pchunks, writer)
                applyNext(writer)
              case Text(txt) =>
                writer.write(txt)
                applyNext(writer)
              case Variable(name) =>
                value match {
                  case view: View => view(name).map {
                    case StringVal(str) =>
                      writer.write(esc(str))
                    case _ =>
                      writer
                  }
                  case StringVal(str) if (Self == name) =>
                    writer.write(esc(str))
                  case _ =>
                    writer
                }
                applyNext(writer)
              case UnescapedVariable(name) =>
                value match {
                  case view: View => view(name).map {
                    case StringVal(str) =>
                      writer.write(str)
                    case _ => writer
                  }
                  case StringVal(str) if (Self == name) =>
                    writer.write(str)
                  case _ => writer
                }
                applyNext(writer)
              case InvertedOpen(name) =>
                val (untilClose, afterClose) = tail.toList.span(!_.isInstanceOf[SectionClose])
                value match {
                  case view: View => view(name) match {
                    case None =>
                      applyNext(writer, standalone)
                    case Some(v: View) =>
                      applyValue(value, afterClose, writer)
                    case Some(Falsy(bool)) =>
                      if (bool) applyValue(value, afterClose, writer)
                      else applyNext(writer, standalone = true)
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
                  case _ => writer
                }
              case SectionOpen(name) =>
                val (untilClose, afterClose) = tail.toList.span {
                  case SectionClose(cname) if (cname == name) => false
                  case _ => true
                }
                value match {
                  case view: View => view(name) match {
                    case None =>
                      applyValue(value, afterClose, writer)
                    case Some(v: View) =>
                      // here we are opening a scope and
                      // need to ensure visibility of
                      // the current scopes members
                      // with a `shared view`
                      applyValue(v.share(view), untilClose, writer)
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
                  case _ =>
                    writer
                }
              case SectionClose(name) =>
                applyNext(writer)
              case Comment(_) => applyNext(writer)
            }
        }
      }
      applyValue(view, chunks, writer)
    }
    override def toString = chunks.toString
  }
}
