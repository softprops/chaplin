package chaplin

object Main {
 def main(args: Array[String]) {
    val template = """
    | test {{#section}}
    | {{greeting}} {{>part}} {{{name}}}
    | {{/section}}""".stripMargin
   def in = new java.io.StringReader(template)
   println("default")
   println(Mustache(in))
   println("custom")
   Mustache.dir("/custom/")(in).fold(identity, { template =>
     println("template %s" format template)
     val writer = new java.io.PrintWriter(System.out)
     template(View().bind("section",
                          View().bind("greeting", StringVal("hi"))
                                .bind("name", StringVal("doug"))
                                .bind("other_partial_var", StringVal("test partials"))),
              writer)
      writer.flush()
   })
  }
}
