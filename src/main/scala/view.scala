package chaplin

trait View {
  def resolve(label: String): Option[Value]
}

trait Value

case class FunctionValue extends Value
case class AnyValue extends Value
case class FalsyValue extends Value
case class IterValue extends Value
