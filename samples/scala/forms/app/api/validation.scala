package play.api.data.validation

object Validation {
  type Mapping[Err, From, To] = (From => Validation[Err, To])
  type Constraint[T] = Mapping[String, T, T]
  type VA[Key, To] = Validation[Seq[(Key, Seq[String])], To]
}

sealed trait Validation[+E, +A]
final case class Success[E, A](a: A) extends Validation[E, A]
final case class Failure[E, A](e: E) extends Validation[E, A]

sealed trait PathNode
case class KeyPathNode(key: String) extends PathNode
case class Path(path: List[KeyPathNode] = List()) {
  import Validation._

  def \(child: String) = Path(path :+ KeyPathNode(child))

  // ((Root \ "age").validate[Int]).apply(mock)
  def apply[From, To](data: From)(implicit m: Path => Mapping[String, From, To]): Validation[String, To] =
    m(this)(data)

}
object Path extends Path(List.empty)

object Mappings {
  import Validation._

  implicit def fromMap(p: Path): Mapping[String, Map[String, Seq[String]], Seq[String]] =
    _.get(p.path.head.key).map(Success.apply).getOrElse(Failure("validation.required"))

  import play.api.libs.json.{ KeyPathNode => JSKeyPathNode, JsPath, JsValue }
  private def pathToJsPath(p: Path): JsPath =
    JsPath(p.path.map(k => JSKeyPathNode(k.key)))

  implicit def fromJson(p: Path): Mapping[String, JsValue, Seq[JsValue]] = { json =>
    pathToJsPath(p)(json) match {
      case Nil => Failure("validation.required")
      case js => Success(js)
    }
  }
}

object Constraints {
  import scala.util.matching.Regex
  import Validation._

  def validateWith[From](msg: String)(pred: From => Boolean): Constraint[From] =
    v => if(!pred(v)) Failure(msg) else Success(v)

  def notEmptyText = validateWith("validation.notemptytext"){ !(_: String).isEmpty }

}