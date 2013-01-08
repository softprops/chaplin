package chaplin

import java.io.Reader

class Mustache(resolver: Resolver) extends Reading with Chunking {
  /** Given a String containing template source, parse it into chunks
   *  of ast elements expanding partial templates where declared. */
  def apply(in: String): Either[String, Template] =
    chunked(in).fold(Left(_), { chunks =>        
      val expanded = ((Right(Nil):Either[String, List[Chunk]]) /: chunks) {
        case (err @ Left(_), _) => err
        case (Right(chunks), Partial(name)) =>
          resolver.resolve(name) match {
            case Some(src) =>
              chunked(src).
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
  /** read partials from the as urls relative to path */
  def dir(path: String) = new Mustache(Resolvers.under(path))
  /** resolve partials as an in-memory lookup map */
  def partials(parts: Map[String, String]) = new Mustache(Resolvers.memory(parts))
}
