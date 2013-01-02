package chaplin

import org.scalatest.FunSpec//FlatSpec
import java.io.StringWriter

import dispatch._
import net.liftweb.json._

class MustacheSpec extends FunSpec {
  val SpecBase = "https://raw.github.com/mustache/spec/master/specs"
  val Features = "interpolation" :: Nil //:: "inverted" :: "interpolation" :: "partials" :: "sections" :: Nil
  val promises = Features map { f =>
    for {
      js <- Http(url(SpecBase) / "%s.json".format(f) > as.lift.Json)
    } yield {
      describe(f) {
        for {
          JObject(fields)                       <- js
          JField("tests", JArray(tests))        <- fields
          JObject(testfields)                   <- tests
          JField("name", JString(name))         <- testfields
          JField("expected", JString(expected)) <- testfields
          JField("template", JString(template)) <- testfields
          JField("desc", JString(desc))         <- testfields
          JField("data", data)                   <- testfields
        } yield {
          it("%s: %s" format(name, desc)) {
            val writer = new StringWriter()
            val view = Views.of(data) match {
              case Some(v:View) => v
              case _ => View()
            }
            Mustache(template).fold(identity, { must =>
              must(view, writer).flush()
              assert(writer.toString() === expected)
            })
          }
        }
      }
    }
  }

  Http.promise.all(promises)()
}
