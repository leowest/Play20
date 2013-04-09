package play.api.data.validation2

import Validations._

trait Extractor[To]{
  def apply[Source](data: Source)(implicit m: Path => Mapping[String, Source, To]): Validation[String, To]
}
sealed trait PathNode
case class KeyPathNode(key: String) extends PathNode
case class Path(path: List[KeyPathNode] = List()) {

  import play.api.libs.functional.syntax._
  import Validation._

  def \(child: String) = Path(path :+ KeyPathNode(child))

  def validate[To]: Extractor[To] = validate(Constraints.noConstraint)

  def validate[To](v: Constraint[To]): Extractor[To] = {
    val path = this
    new Extractor[To] {
      def apply[Source](data: Source)(implicit m: Path => Mapping[String, Source, To]): Validation[String, To] = path(data).flatMap(v)
    }
  }

}
object Path extends Path(List.empty)

object Extractors {

  // TODO: recursive map traversal
  implicit def pickAll(p: Path): Mapping[String, Map[String, Seq[String]], Seq[String]] =
    _.get(p.path.head.key).map(Success.apply[String, Seq[String]] _).getOrElse(Failure(Seq("validation.required")))
  implicit def pickFirst(p: Path): Mapping[String, Map[String, Seq[String]], String] = data =>
    pickAll(p)(data).map(_.head)


  import play.api.libs.json.{ KeyPathNode => JSKeyPathNode, _ }
  private def pathToJsPath(p: Path): JsPath =
    JsPath(p.path.map(k => JSKeyPathNode(k.key)))

  implicit def pickJson(p: Path): Mapping[String, JsValue, Seq[JsValue]] = { json =>
    pathToJsPath(p)(json) match {
      case Nil => Failure(Seq("validation.required"))
      case js => Success(js)
    }
  }

  implicit def pickStringInJson(p: Path): Mapping[String, JsValue, String] = { json =>
    pickJson(p)(json).flatMap {
      case JsString(v) :: _ => Success(v)
      case _ => Failure(Seq("validation.type-mismatch"))
    }
  }
}

object Constraints {
  import scala.util.matching.Regex
  import Validation._

  def validateWith[From](msg: String)(pred: From => Boolean): Constraint[From] =
    v => if(!pred(v)) Failure(Seq(msg)) else Success(v)

  def notEmptyText = validateWith("validation.notemptytext"){ !(_: String).isEmpty }
  def noConstraint[From]: Constraint[From] = Success(_)

}