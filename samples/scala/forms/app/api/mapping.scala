package play.api.data.validation2

import scala.language.higherKinds

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
    Rule(this, (p: Path[I]) => (d: I) => m(p)(d).fail.map{ errs => Seq(p -> errs) }, v) // XXX: DRY "fail.map" thingy

  def validate[O](sub: Rule[I, O])(implicit l: Path[I] => Mapping[String, I, I]): Rule[I, O] = {
    val parent = this
    Rule(this, { p => d =>
      val v = l(parent)(d)
      v.fold(
        es => Failure(Seq(parent -> es)),
        s  => sub.m(p)(s))
    }, sub.v)
  }

  def validate[O](implicit m: Path[I] => Mapping[String, I, O]): Rule[I, O] =
    validate(Constraints.noConstraint[O])

  override def toString = "Path \\ " + path.mkString(" \\ ")
}

object JsPath extends Path[play.api.libs.json.JsValue](Nil)
object FormPath extends Path[Map[String, Seq[String]]](Nil)

object Mappings {

  import play.api.mvc.Request

  implicit def seqAsO[O](implicit m: Mapping[String, String, O]): Mapping[String, Seq[String], O] =
    _.headOption.map(Success[String, String](_)).getOrElse(Failure[String, String](Seq("validation.required"))).flatMap(m)

  implicit def stringAsInt: Mapping[String, String, Int] =
    Constraints.validateWith("validation.type-mismatch"){ (_: String).matches("-?[0-9]+") }(_).map(_.toInt)

  import play.api.libs.json.{ KeyPathNode => JSKeyPathNode, _ }
  private def pathToJsPath(p: Path[JsValue]) =
    play.api.libs.json.JsPath(p.path.map(k => JSKeyPathNode(k.key)))


  implicit def IasI[I]: Mapping[String, I, I] = Success(_)

  implicit def jsonAsString: Mapping[String, JsValue, String] = {
    case JsString(v) => Success(v)
    case _ => Failure(Seq("validation.type-mismatch"))
  }

  implicit def jsonAsInt: Mapping[String, JsValue, Int] = {
    case JsNumber(v) => Success(v.toInt)
    case _ => Failure(Seq("validation.type-mismatch"))
  }

  implicit def jsonAsSeq[O](implicit m: Mapping[String, JsValue, O]): Mapping[String, JsValue, Seq[O]] = {
    case JsArray(vs) => Validation.sequence(vs.map(m))
    case _ => Failure(Seq("validation.type-mismatch"))
  }

/*
  // XXX: diverging implicit issue (validation is covariant on A)
  implicit def jsonAsList[O](implicit m: Mapping[String, JsValue, O]): Mapping[String, JsValue, List[O]] =
    jsonAsSeq[O](m)(_).map(_.toList)
*/

  implicit def pickInRequest[I, O](p: Path[Request[I]])(implicit pick: Path[I] => Mapping[String, I, O]): Mapping[String, Request[I], O] =
    request => pick(Path[I](p.path))(request.body)

  implicit def pickOptional[I, O](p: Path[I])(implicit pick: Path[I] => Mapping[String, I, O]): Mapping[String, I, Option[O]] =
    d => pick(p)(d).map(Some.apply) | Success(None)

  implicit def pickInJson[O](p: Path[JsValue])(implicit m: Mapping[String, JsValue, O]): Mapping[String, JsValue, O] = { json =>
    val v: Validation[String, JsValue] = pathToJsPath(p)(json) match {
      case Nil => Failure(Seq("validation.required"))
      case js :: _ => Success(js)
    }
    v.flatMap(m)
  }

  implicit def pickInMap[O](p: Path[Map[String, Seq[String]]])(implicit m: Mapping[String, Seq[String], O]): Mapping[String, Map[String, Seq[String]], O] = {
    data =>
      val key = p.path.map(_.key).mkString(".")
      val validation: Validation[String, Seq[String]] =
        data.get(key).map(Success[String, Seq[String]](_)).getOrElse{ Failure[String, Seq[String]](Seq("validation.required")) }
      validation.flatMap(m)
  }

  implicit def mapPickMap(p: Path[Map[String, Seq[String]]]): Mapping[String, Map[String, Seq[String]], Map[String, Seq[String]]] = { m =>
    val prefix = p.path.map(_.key).mkString(".") + "."
    val submap = m.filterKeys(_.startsWith(prefix)).map { case (k, v) =>
      k.substring(prefix.length) -> v
    }
    Success(submap)
  }

}

object Constraints {
  import scala.util.matching.Regex

  def validateWith[From](msg: String)(pred: From => Boolean): Constraint[From] =
    v => if(!pred(v)) Failure(Seq(msg)) else Success(v)

  def optional[O](c: Constraint[O]): Constraint[Option[O]] =
    _.map(v => c(v).map(Some.apply)).getOrElse(Success(None))

  def seq[O](c: Constraint[O]): Constraint[Seq[O]] =
    vs => Validation.sequence(vs.map(c))

  def seq[I, O](r: Rule[I, O])(implicit m: Mapping[String, I, Seq[I]]): Rule[I, Seq[O]] =
    Rule(Path[I](), { p => d =>
      m(d).fold(
        errs => Failure(Seq(p -> errs)),
        is => Validation.sequence(is.map(r.m(p))))
    }, seq(r.v))

  def list[O](c: Constraint[O]): Constraint[List[O]] =
    seq(c)(_).map(_.toList)

  def nonEmptyText = validateWith("validation.nonemptytext"){ !(_: String).isEmpty }
  def min(m: Int) = validateWith("validation.min"){(_: Int) > m}
  def max(m: Int) = validateWith("validation.max"){(_: Int) < m}
  def minLength(l: Int) = validateWith("validation.minLength"){(_: String).size >= l}
  def maxLength(l: Int) = validateWith("validation.maxLength"){(_: String).size < l}
  def pattern(regex: Regex) = validateWith("validation.pattern"){regex.unapplySeq(_: String).isDefined}
  def email = pattern("""\b[a-zA-Z0-9.!#$%&’*+/=?^_`{|}~-]+@[a-zA-Z0-9-]+(?:\.[a-zA-Z0-9-]+)*\b""".r)(_: String).fail.map(_ => Seq("validation.email"))
  def noConstraint[From]: Constraint[From] = Success(_)
}