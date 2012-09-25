package chaplin

import java.io.{ BufferedReader, InputStreamReader, Reader }

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

class Mustache(resolver: Resolver) extends Reading with Chunking {
  /** Given a String containing template source, parse it into chunks
   *  of ast elements expanding partial templates where declared. */
  def apply(in: String): Either[String, Template] =
    chunked(in).fold(Left(_), { chunks =>        
      val expanded = ((Right(Nil):Either[String, List[Chunk]]) /: chunks) {
        case (err @ Left(_), _) => err
        case (Right(chunks), Partial(name)) =>
          resolver.resolve(name) match {
            case Some(url) =>
              chunked(readAll(new InputStreamReader(url.openStream()))).
                fold(Left(_), {
                  partialChunks =>
                    Right(partialChunks ::: chunks)
                })
            case _ => Left("unresolved partial template: '%s'" format name)
          }
        case (eth, c) => eth.fold(Left(_), {
          chunks => Right(c :: chunks)
        })
      }
      expanded.fold(Left(_), { chunks =>
        Right(Templates.mkTemplate(chunks.reverse))
      })
    })

  def apply(reader: Reader): Either[String, Template] =    
    apply(readAll(reader))
}

object Mustache extends Mustache(Resolvers) {
  def dir(path: String) = new Mustache(Resolvers.under(path))
}
