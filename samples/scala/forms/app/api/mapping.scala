package play.api.data.validation2

import scala.language.implicitConversions
import Validations._

case class Rule[I, O](p: Path[I], m: Path[I] => Mapping[(Path[I], Seq[String]), I, O], v: Constraint[O] = Constraints.noConstraint[O]) {
  def validate(data: I): VA[I, O] =
    m(p)(data).flatMap(v(_).fail.map{ errs => Seq(p -> errs) })
}

sealed trait PathNode
case class KeyPathNode(key: String) extends PathNode
case class Path[I](path: List[KeyPathNode] = Nil) {

  def \(child: String): Path[I] = this \ KeyPathNode(child)
  def \(child: KeyPathNode): Path[I] = Path(path :+ child)

  def compose(p: Path[I]): Path[I] = Path(this.path ++ p.path)

  def validate[O](v: Constraint[O])(implicit m: Path[I] => Mapping[String, I, O]): Rule[I, O] =
    Rule(this, (p: Path[I]) => (d: I) => m(p)(d).fail.map{ errs => Seq(p -> errs) }, v)

  def validate[O](sub: Rule[I, O]): Rule[I, O] =
    Rule(this compose sub.p, sub.m, sub.v)

  def validate[O](implicit m: Path[I] => Mapping[String, I, O]): Rule[I, O] =
    validate(Constraints.noConstraint[O])

  override def toString = "Path \\ " + path.mkString(" \\ ")
}

object JsPath extends Path[play.api.libs.json.JsValue](Nil)
object FormPath extends Path[Map[String, Seq[String]]](Nil)

object Mappings {
  //TODO: refactor
  import play.api.mvc.Request

  implicit def mapPickMap(p: Path[Map[String, Seq[String]]]): Mapping[String, Map[String, Seq[String]], Map[String, Seq[String]]] = { m =>
    val prefix = p.path.map(_.key).mkString(".") + "."
    val submap = m.filterKeys(_.startsWith(prefix)).map { case (k, v) =>
      k.substring(prefix.length) -> v
    }
    Success(submap)
  }

  implicit def mapPickStrings(p: Path[Map[String, Seq[String]]]): Mapping[String, Map[String, Seq[String]], Seq[String]] =
    _.get(p.path.map(_.key).mkString(".")).map(Success.apply[String, Seq[String]] _).getOrElse(Failure(Seq("validation.required")))

  implicit def mapPickString(p: Path[Map[String, Seq[String]]]): Mapping[String, Map[String, Seq[String]], String] =
    data => mapPickStrings(p)(data).map(_.head)
  implicit def mapPickInt(p: Path[Map[String, Seq[String]]]): Mapping[String, Map[String, Seq[String]], Int] =
    data => mapPickString(p)(data).flatMap(Constraints.validateWith("validation.int"){ (_: String).matches("-?[0-9]+") }).map(_.toInt)

  import play.api.libs.json.{ KeyPathNode => JSKeyPathNode, _ }
  private def pathToJsPath(p: Path[JsValue]) =
    play.api.libs.json.JsPath(p.path.map(k => JSKeyPathNode(k.key)))

  implicit def jsonPickJsons(p: Path[JsValue]): Mapping[String, JsValue, Seq[JsValue]] = { json =>
    pathToJsPath(p)(json) match {
      case Nil => Failure(Seq("validation.required"))
      case js => Success(js)
    }
  }

  implicit def jsonPickJson(p: Path[JsValue]): Mapping[String, JsValue, JsValue] = json =>
    jsonPickJsons(p)(json).map(_.head)

  implicit def jsonPickOne[O](p: Path[JsValue])(implicit m: Mapping[String, JsValue, O]): Mapping[String, JsValue, O] = { json =>
    jsonPickJson(p)(json).flatMap(m)
  }

  implicit def jsonAsString: Mapping[String, JsValue, String] = {
    case JsString(v) => Success(v)
    case _ => Failure(Seq("validation.type-mismatch"))
  }

  implicit def jsonAsInt: Mapping[String, JsValue, Int] = {
    case JsNumber(v) => Success(v.toInt)
    case _ => Failure(Seq("validation.type-mismatch"))
  }

  implicit def jsonPickSeqs[O](p: Path[JsValue])(implicit m: Mapping[String, JsValue, O]): Mapping[String, JsValue, Seq[O]] = { json =>
    jsonPickJson(p)(json).flatMap {
      case JsArray(vs) => Validation.sequence(vs.map(m))
      case _ => Failure(Seq("validation.type-mismatch"))
    }
  }

  implicit def pickInRequest[I, O](p: Path[Request[I]])(implicit pick: Path[I] => Mapping[String, I, O]): Mapping[String, Request[I], O] = { request =>
    pick(Path[I](p.path))(request.body)
  }

  implicit def pickOptional[I, O](p: Path[I])(implicit pick: Path[I] => Mapping[String, I, O]): Mapping[String, I, Option[O]] = { d =>
    pick(p)(d).map(Some.apply) | Success(None)
  }

}

object Constraints {
  import scala.util.matching.Regex

  def validateWith[From](msg: String)(pred: From => Boolean): Constraint[From] =
    v => if(!pred(v)) Failure(Seq(msg)) else Success(v)

  def optional[O](c: Constraint[O]): Constraint[Option[O]] =
    _.map(v => c(v).map(Some.apply)).getOrElse(Success(None))

  def list[O](c: Constraint[O]): Constraint[Seq[O]] =
    vs => Validation.sequence(vs.map(c))

  def nonEmptyText = validateWith("validation.nonemptytext"){ !(_: String).isEmpty }
  def min(m: Int) = validateWith("validation.min"){(_: Int) > m}
  def max(m: Int) = validateWith("validation.max"){(_: Int) < m}
  def minLength(l: Int) = validateWith("validation.minLength"){(_: String).size >= l}
  def maxLength(l: Int) = validateWith("validation.maxLength"){(_: String).size < l}
  def pattern(regex: Regex) = validateWith("validation.pattern"){regex.unapplySeq(_: String).isDefined}
  def email = pattern("""\b[a-zA-Z0-9.!#$%&’*+/=?^_`{|}~-]+@[a-zA-Z0-9-]+(?:\.[a-zA-Z0-9-]+)*\b""".r)(_: String).fail.map(_ => Seq("validation.email"))
  def noConstraint[From]: Constraint[From] = Success(_)
}