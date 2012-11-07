package chaplin

import java.io.{ PrintWriter, StringReader }
object Main {
 def main(args: Array[String]) {
   //http://mustache.github.com/mustache.5.html

   val typical = """Hello {{name}}
   |You have just won ${{value}}!
   |{{#in_ca}}
   |Well, ${{taxed_value}}, after taxes.
   |{{/in_ca}}""".stripMargin

   Mustache(typical).fold(identity, { template =>
     val writer = new java.io.PrintWriter(System.out)
     val view = View().bind("name", StringVal("chris"))
                      .bind("value", StringVal("10000"))
                      .bind("in_ca", Falsy(true))
                      .bind("taxed_value", StringVal((10000 - (10000 * 0.4)).toString))
     template(view, writer)
     writer.flush()
   })

   val inverted = """{{#repo}}
   |<b>{{name}}</b>
   |{{/repo}}
   |{{^repo}}
   |No repos :(
   |{{/repo}}""".stripMargin

   Mustache(inverted).fold(identity, { template =>
     val writer = new PrintWriter(System.out)

     // with repo
     val view = View().bind("repo", View().bind("name", StringVal("chaplin")))
     template(view, writer)
     writer.flush()

     // without repo
     template(View(), writer)
     writer.flush()
   })

 }
}
