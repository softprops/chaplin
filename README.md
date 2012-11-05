# chaplin

![http://upload.wikimedia.org/wikipedia/commons/thumb/0/00/Charlie_Chaplin.jpg/225px-Charlie_Chaplin.jpg](http://upload.wikimedia.org/wikipedia/commons/thumb/0/00/Charlie_Chaplin.jpg/225px-Charlie_Chaplin.jpg)

Nothing to worth gawking over (yet). Just playing with parser combinators checking out my mustache in the mirror. (View binding is not currently flushed out.)

Dependencies: 0.

## usage

    import chaplin._
    import java.io.PrintWriter
    val in """
    | say {{greeting}} to
    | {{#person}}
    |   {{name}}
    | {{/person}}
    """.stripMargin
    val writer = new PrintWriter(System.out)
    Mustache(in)(View().bind("greeting", StringVal("hello"))
                       .bind("person", View().bind("name", "charley")),
                       writer)
    writer.flush()

Doug Tangren (softprops) 2012
