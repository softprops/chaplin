package chaplin

sealed trait Value
abstract class FuncVal extends Function0[Value] with Value
case class StringVal(value: String) extends Value
case class Falsy(value: Boolean) extends Value
case class IterableVal(values: Iterable[Value]) extends Value
case class View(
  bindings: Map[String, Value] = Map.empty[String, Value],
  parent: Option[View] = None) extends Value {
  /** binds a value to a label. If the value is a view, tag it with
   *  this as its parent */
  def bind(label: String, value: Value): View =
    copy(bindings = bindings + (label -> value)).reparent

  /** shared views */
  def share(view: View) =
    copy(bindings ++ view.bindings)

  /** update all of the current view's binding references to parent */
  private def reparent: View =
    copy(bindings = (bindings /: bindings) {
      case (b, (label, view: View)) =>
        b + (label -> view.copy(parent = Some(this)).reparent)
      case (b, _) =>
        b
    })

  /** resolve a label within this view or its parent view */
  def apply(label: String): Option[Value] =
    if (label.contains(".")) {
      label.span(_ != '.') match {
        case (lab, rest) => apply(lab).flatMap {
          case v: View => v.apply(rest.drop(1))
          case value => Some(value)
        }
      }
    } else bindings.get(label).orElse(parent.flatMap(_(label)))
}


