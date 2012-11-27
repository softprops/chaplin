# chaplin

![http://upload.wikimedia.org/wikipedia/commons/thumb/0/00/Charlie_Chaplin.jpg/225px-Charlie_Chaplin.jpg](http://upload.wikimedia.org/wikipedia/commons/thumb/0/00/Charlie_Chaplin.jpg/225px-Charlie_Chaplin.jpg)

You may not remember the name but you will remember the mustache.

A no BS, 0 dependency mustache implementation you can take home to your parents.

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
