package play.api.data.validation2

import scala.language.implicitConversions
import Validations._

// XXX: This is almost a Mapping
trait Extractor[S, A]{
  type F = Path => Mapping[String, S, A]
  def apply(data: S)(implicit m: F): Validation[(Path, Seq[String]), A]
}

object Extractor {
  def apply[S, A](p: Path, v: Constraint[A]) = new Extractor[S, A] {
    def apply(data: S)(implicit m: F) =
      m(p)(data).flatMap(v).fail.map(errs => Seq(p -> errs))
  }

  def apply[S, A](parent: Path, sub: Extractor[S, A]) = new Extractor[S, A] {
    def apply(data: S)(implicit m: F) =
      sub(data){ path => m(parent.compose(path)) }
  }
}

sealed trait PathNode
case class KeyPathNode(key: String) extends PathNode
case class Path(path: List[KeyPathNode] = List()) {

  import play.api.libs.functional.syntax._

  def \(child: String): Path = this \ KeyPathNode(child)
  def \(child: KeyPathNode): Path = Path(path :+ child)

  def compose(p: Path): Path = Path(this.path ++ p.path)

  def validate[From, To]: Extractor[From, To] = validate(Constraints.noConstraint[To])
  def validate[From, To](v: Constraint[To]): Extractor[From, To] = Extractor(this, v)
  def validate[From, To](sub: Extractor[From, To]): Extractor[From, To] = Extractor(this, sub)

  override def toString = "Path \\ " + path.mkString(" \\ ")
}

object Path extends Path(List.empty)

object Extractors {

  implicit def mapPickStrings(p: Path): Mapping[String, Map[String, Seq[String]], Seq[String]] =
    _.get(p.path.map(_.key).mkString(".")).map(Success.apply[String, Seq[String]] _).getOrElse(Failure(Seq("validation.required")))

  implicit def mapPickString(p: Path): Mapping[String, Map[String, Seq[String]], String] =
    data => mapPickStrings(p)(data).map(_.head)
  implicit def mapPickInt(p: Path): Mapping[String, Map[String, Seq[String]], Int] =
    data => mapPickString(p)(data).flatMap(Constraints.validateWith("validation.int"){ (_: String).matches("-?[0-9]+") }).map(_.toInt)

  import play.api.libs.json.{ KeyPathNode => JSKeyPathNode, _ }
  private def pathToJsPath(p: Path): JsPath =
    JsPath(p.path.map(k => JSKeyPathNode(k.key)))

  implicit def jsonPickJson(p: Path): Mapping[String, JsValue, Seq[JsValue]] = { json =>
    pathToJsPath(p)(json) match {
      case Nil => Failure(Seq("validation.required"))
      case js => Success(js)
    }
  }

  implicit def jsonPickString(p: Path): Mapping[String, JsValue, String] = { json =>
    jsonPickJson(p)(json).flatMap {
      case JsString(v) :: _ => Success(v)
      case _ => Failure(Seq("validation.type-mismatch"))
    }
  }

  implicit def jsonPickInt(p: Path): Mapping[String, JsValue, Int] = { json =>
    jsonPickJson(p)(json).flatMap {
      case JsNumber(v) :: _ => Success(v.toInt)
      case _ => Failure(Seq("validation.int"))
    }
  }

}

object Constraints {
  import scala.util.matching.Regex

  def validateWith[From](msg: String)(pred: From => Boolean): Constraint[From] =
    v => if(!pred(v)) Failure(Seq(msg)) else Success(v)

  def nonEmptyText = validateWith("validation.nonemptytext"){ !(_: String).isEmpty }
  def min(m: Int) = validateWith("validation.min"){(_: Int) > m}
  def max(m: Int) = validateWith("validation.max"){(_: Int) < m}
  def minLength(l: Int) = validateWith("validation.minLength"){(_: String).size >= l}
  def maxLength(l: Int) = validateWith("validation.maxLength"){(_: String).size < l}
  def pattern(regex: Regex) = validateWith("validation.pattern"){regex.unapplySeq(_: String).isDefined}
  def email = pattern("""\b[a-zA-Z0-9.!#$%&’*+/=?^_`{|}~-]+@[a-zA-Z0-9-]+(?:\.[a-zA-Z0-9-]+)*\b""".r)(_: String).fail.map(_ => Seq("validation.email"))
  def noConstraint[From]: Constraint[From] = Success(_)
}