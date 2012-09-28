package chaplin

import java.io.{ BufferedReader, Reader }

trait Reading {
  def readAll(reader: Reader): String = {
    @annotation.tailrec
    def read(br: BufferedReader, ls: List[String]): String =
      br.readLine match {
        case null =>
          br.close()
          ls.reverse.mkString("\n")
        case line =>
          read(br, line :: ls)
      }
    read(new BufferedReader(reader, 1024), Nil)
  }
}

trait Chunking {
  def chunked(in: String): Either[String, List[Chunk]] = {
    Parse(in) match {
      case Parse.Success(chunks, _) => Right(chunks)
      case _ => Left("malformed template source: %s" format in)
    }
  }
}
