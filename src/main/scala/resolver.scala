package chaplin

import java.net.URL
import java.io.InputStreamReader

trait Resolver {
  def resolve(name: String): Option[String]
}

case class MemoryResolver(partials: Map[String, String]) extends Resolver {
  def resolve(name: String) = partials.get(name)
}

class URLResolver(path: String) extends Resolver with Reading {
  def resolve(name: String): Option[String] =
    getClass().getResource(expanded(name)) match {
      case null =>
        println("%s did not exist for %s" format(expanded(name), getClass().getName()))
        None
      case url if (exists(url)) =>
        Some(readAll(new InputStreamReader(url.openStream())))
      case _ =>
        None
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

object Resolvers extends URLResolver("/") {
  def under(dir: String): Resolver = new URLResolver(dir)
  def memory(parts: Map[String, String]) = MemoryResolver(parts)
}
