package chaplin

object Main {
 def main(args: Array[String]) {
    val template = """
    | test {{#section}}
    | {{hi}} {{>part}} {{{doug}}}
    | {{/section}}""".stripMargin
   def in = new java.io.StringReader(template)
   println("default")
   println(Mustache(in))
   println("custom")
   println(Mustache.dir("/custom/")(in))
  }
}
