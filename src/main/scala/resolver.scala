package chaplin

import java.net.URL

class Resolver(path: String) {
  def resolve(name: String): Option[URL] =
    getClass().getResource(expanded(name)) match {
      case null => println("%s did not exist for %s" format(expanded(name), getClass().getName())); None
      case url if (exists(url)) => println(url);println(path); Some(url)
      case _ => None
    }

  private def expanded(name: String) =
    qualified(withExt(name))

  private def withExt(name: String) =
    if (name.endsWith(".mustache")) name
    else "%s.mustache" format(name)

  private def qualified(name: String) =
    if (name.startsWith(path)) name
    else "%s%s".format(path, name)                          

   // fixme: is there a better way?
  private def exists(u: URL) =
    try   { u.openStream().close(); true }
    catch { case e => e.printStackTrace(); false }
}

object Resolvers extends Resolver("/") {
  def under(dir: String): Resolver = new Resolver(dir)
}
