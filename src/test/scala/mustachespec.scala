package chaplin

import org.scalatest.FlatSpec
import java.io.StringWriter

class MustacheSpec extends FlatSpec {
  "mustache" should "render basic template" in {
    val typical = """Hello {{name}}
    |You have just won ${{value}}!
    |{{#in_ca}}
    |Well, ${{taxed_value}}, after taxes.
    |{{/in_ca}}""".stripMargin
    val writer = new StringWriter()
    Mustache(typical).fold(identity, { template =>
      val view = View().bind("name", StringVal("chris"))
                      .bind("value", StringVal("10000"))
                      .bind("in_ca", Falsy(true))
                      .bind("taxed_value", StringVal((10000 - (10000 * 0.4)).toString))
     template(view, writer).flush()
     assert(writer.toString() === """Hello chris
            |You have just won $10000!
            |
            |Well, $6000.0, after taxes.
            |""".stripMargin)
   })
  }

  it should "render an inverted template"  in  {
    val inverted = """{{#repo}}
    |<b>{{name}}</b>
    |{{/repo}}
    |{{^repo}}
    |No repos :(
    |{{/repo}}""".stripMargin

    val invertedStache = Mustache(inverted)
    val writer = new StringWriter()
    invertedStache.fold(identity, { template =>
      val view = View().bind("repo", View().bind("name", StringVal("chaplin")))
      template(view, writer)
    })
    assert(writer.toString() === """
           |<b>chaplin</b>
           |
           |""".stripMargin)
  }

  it should "not render comments" in {
    val comment = "<h1>Today{{! ignore me }}.</h1>"
    val writer = new StringWriter()
    Mustache(comment).fold(identity, { template =>
      template(View(), writer).flush()
    })
    assert(writer.toString() === "<h1>Today.</h1>")
  }
}
