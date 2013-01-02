package chaplin

import net.liftweb.json._

object Views {
  def of(jv: JValue, view: Option[Value] = Some(View())): Option[Value] = jv match {
    case JObject(fields) =>
      val mapped = fields.map {
        case JField(name, value) =>
          (name, of(value, view))
      }
      view.flatMap {
        case v: View =>
          Some((v /: mapped){
            case (view, (name, Some(value))) =>
              view.bind(name, value)
            case (view, _) =>
              view
          })        
      }
    case JArray(ary)  =>
      Some(IterableVal(ary.map(v => of(v, view)).flatten))
    case JNothing     => None
    case JBool(bool)  => Some(Falsy(bool))
    case JString(str) => Some(StringVal(str))
    case JDouble(dub) => Some(StringVal(dub.toString))
    case JInt(int)    => Some(StringVal(int.toString))
    case JField(name, value) =>
      view.flatMap {
        case v: View =>
          of(value, view).map(v.bind(name, _))
      }
  }
}

