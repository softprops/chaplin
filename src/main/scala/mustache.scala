package chaplin

import java.io.Reader

class Mustache(resolver: Resolver) extends Reading with Chunking {

  /** Represents ast element and map of partial name to partial ast */
  type Compilation = (List[Chunk], Map[String, List[Chunk]])

  /** Given a String containing template source, parse it into chunks
   *  of ast elements expanding partial templates where declared. */
  def apply(in: String): Either[String, Template] =
    chunked(in).fold(Left(_), { chunks =>
      /** expands a  tree of compilation chunks, pk may be a partial key whose tree is being expanded */
      def expanded(cx: List[Chunk], pk: String = ""): Either[String, Compilation] = {
        ((Right((Nil, Map.empty[String, List[Chunk]])):Either[String, Compilation]) /: cx) {
          case (err @ Left(_), _) => err
          case (compiled @ Right((chunks, partials)), p @ Partial(name)) if (pk != name) =>
            partials.get(name)
             .map(_ => Right((p :: chunks, partials)))
             .getOrElse {
               resolver.resolve(name) match {
                 case Some(src) =>
                   chunked(src).
                     fold(Left(_), {
                       partialChunks =>
                         expanded(partialChunks, name).fold(Left(_), {
                           case (xpc, xparts) =>
                             Right((p :: chunks, xparts ++ partials + (name -> xpc.reverse)))
                         })
                     })
                 case _ =>
                   println("unresolved partial template: '%s'" format name)
                   compiled
               }
             }
          case (eth, c) => eth.fold(Left(_), {
            case (chunks, partials) => Right((c :: chunks, partials))
          })
        }
      }
      expanded(chunks).fold(Left(_), {
        case (chunks, partials) => Right(Templates.mkTemplate(chunks.reverse, partials))
      })
    })

  /** apply for any java reader */
  def apply(reader: Reader): Either[String, Template] =    
    apply(readAll(reader))
}

object Mustache extends Mustache(Resolvers) {
  /** read partials from the as urls relative to path */
  def dir(path: String) = new Mustache(Resolvers.under(path))
  /** resolve partials as an in-memory lookup map */
  def partials(parts: Iterable[(String, String)]) = new Mustache(Resolvers.memory(parts.toMap))
}
