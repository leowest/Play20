package play.api.data.validation2

import scala.language.implicitConversions
import Validations._

sealed trait PathNode
case class KeyPathNode(key: String) extends PathNode
case class Path(path: List[KeyPathNode] = List()) {

  import play.api.libs.functional.syntax._

  def \(child: String): Path = this \ KeyPathNode(child)
  def \(child: KeyPathNode): Path = Path(path :+ child)

  def compose(p: Path): Path = Path(this.path ++ p.path)

  def validate[From, To](data: From)(implicit m: Path => Mapping[String, From, To]): Validation[(Path, Seq[String]), To] =
    validate(Constraints.noConstraint[To])(data)

  def validate[From, To](v: Constraint[To])(data: From)(implicit m: Path => Mapping[String, From, To]): Validation[(Path, Seq[String]), To] =
    m(this)(data).flatMap(v).fail.map(errs => Seq(this -> errs))

  def validateSub[From, To](sub: Mapping[(Path, Seq[String]), From, To])(data: From)(implicit m: Path => Mapping[String, From, To],  e: Path => Mapping[String, From, From]): Validation[(Path, Seq[String]), To] =
    this.validate[From, From](data).flatMap(sub(_))

  override def toString = "Path \\ " + path.mkString(" \\ ")
}

object Path extends Path(List.empty)

object Extractors {

  implicit def mapPickMap(p: Path): Mapping[String, Map[String, Seq[String]], Map[String, Seq[String]]] = { m =>
    val prefix = p.path.map(_.key).mkString(".") + "."
    val submap = m.filterKeys(_.startsWith(prefix)).map { case (k, v) =>
      k.substring(prefix.length) -> v
    }
    Success(submap)
  }

  implicit def mapPickStrings(p: Path): Mapping[String, Map[String, Seq[String]], Seq[String]] =
    _.get(p.path.map(_.key).mkString(".")).map(Success.apply[String, Seq[String]] _).getOrElse(Failure(Seq("validation.required")))

  implicit def mapPickString(p: Path): Mapping[String, Map[String, Seq[String]], String] =
    data => mapPickStrings(p)(data).map(_.head)
  implicit def mapPickInt(p: Path): Mapping[String, Map[String, Seq[String]], Int] =
    data => mapPickString(p)(data).flatMap(Constraints.validateWith("validation.int"){ (_: String).matches("-?[0-9]+") }).map(_.toInt)

  import play.api.libs.json.{ KeyPathNode => JSKeyPathNode, _ }
  private def pathToJsPath(p: Path): JsPath =
    JsPath(p.path.map(k => JSKeyPathNode(k.key)))

  implicit def jsonPickJsons(p: Path): Mapping[String, JsValue, Seq[JsValue]] = { json =>
    pathToJsPath(p)(json) match {
      case Nil => Failure(Seq("validation.required"))
      case js => Success(js)
    }
  }

  implicit def jsonPickJson(p: Path): Mapping[String, JsValue, JsValue] = json =>
    jsonPickJsons(p)(json).map(_.head)

  implicit def jsonPickString(p: Path): Mapping[String, JsValue, String] = { json =>
    jsonPickJsons(p)(json).flatMap {
      case JsString(v) :: _ => Success(v)
      case _ => Failure(Seq("validation.type-mismatch"))
    }
  }

  implicit def jsonPickInt(p: Path): Mapping[String, JsValue, Int] = { json =>
    jsonPickJsons(p)(json).flatMap {
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