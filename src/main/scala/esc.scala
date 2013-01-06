package chaplin

object Esc {
  import scala.util.matching.Regex
  val Replacements = Map(
    "&"  -> "&amp;",
    "\"" -> "&quot;",
    "'"  -> "&#39;",
    "<"  -> "&lt;",
    ">"  -> "&gt;"
  )
  private val Pat =
    new Regex("(%s)" format Replacements.keys.mkString("|"),
              "ent")

  def apply(in: String) =
    Pat replaceAllIn(in, m => Replacements(m group "ent"))
}
