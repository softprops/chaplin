package chaplin

sealed trait Value
case class StringVal(value: String) extends Value
case class Falsy(value: Boolean) extends Value
case class IterableVal(values: Iterable[Value]) extends Value
case class View(
  bindings: Map[String, Value] = Map.empty[String, Value],
  parent: Option[View] = None) extends Value {
  /** binds a value to a label. If the value is a view, tag it with
   *  this as its parent */
  def bind(label: String, value: Value) =
    value match {
      case view:View =>
        copy(bindings =
          bindings + (label -> view.copy(parent = Some(this))))
      case _ =>
        copy(bindings = bindings + (label -> value))
    }
  /** resolve a label within this view or its parent view */
  def apply(label: String): Option[Value] =
    bindings.get(label).orElse(parent.flatMap(_(label)))
}


