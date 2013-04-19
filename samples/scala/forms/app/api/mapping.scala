package play.api.data.validation2

import scala.language.higherKinds
import scala.language.implicitConversions

import Validations._

case class Rule[I, O](p: Path[I], m: Path[I] => Mapping[(Path[I], Seq[String]), I, O], v: Constraint[O] = Constraints.noConstraint[O]) {
  def validate(data: I): VA[I, O] =
    m(p)(data).flatMap(v(_).fail.map{ errs => Seq(p -> errs) })
}

sealed trait PathNode
case class KeyPathNode(key: String) extends PathNode {
  override def toString = key
}

case class IdxPathNode(idx: Int) extends PathNode {
  override def toString = s"[$idx]"
}

object \: {
  def unapply[I](path: Path[I]): Option[(PathNode, Path[I])] = {
    path match {
      case Path(Nil) => None
      case Path(n :: ns) => Some(n -> Path[I](ns))
    }
  }
}

case class Path[I](path: List[PathNode] = Nil) {

  def \(key: String): Path[I] = this \ KeyPathNode(key)
  def \(idx: Int): Path[I] = this \ IdxPathNode(idx)
  def \(child: PathNode): Path[I] = Path(path :+ child)

  def as[J] = Path[J](path)

  def compose(p: Path[I]): Path[I] = Path(this.path ++ p.path)

  def validate[O](v: Constraint[O])(implicit m: Path[I] => Mapping[String, I, O]): Rule[I, O] =
    Rule(this, (p: Path[I]) => (d: I) => m(p)(d).fail.map{ errs => Seq(p -> errs) }, v) // XXX: DRY "fail.map" thingy

  def validate[J, O](sub: Rule[J, O])(implicit l: Path[I] => Mapping[String, I, J]): Rule[I, O] = {
    val parent = this
    Rule(parent compose Path[I](sub.p.path), { p => d =>
      val v = l(parent)(d)
      v.fold(
        es => Failure(Seq(parent -> es)),
        s  => sub.m(sub.p)(s)).fail.map{ _.map {
          case (path, errs) => (parent compose Path[I](path.path)) -> errs
        }}
    }, sub.v)
  }

  def validate[O](implicit m: Path[I] => Mapping[String, I, O]): Rule[I, O] =
    validate(Constraints.noConstraint[O])

  override def toString = this.path match {
    case Nil => "/"
    case hs => hs.foldLeft("") {
      case (path, IdxPathNode(i)) => path + s"[$i]"
      case (path, KeyPathNode(k)) => path + "/" + k
    }
  }
}

object JsPath extends Path[play.api.libs.json.JsValue](Nil)
object FormPath extends Path[Map[String, Seq[String]]](Nil)

object Mappings {

  import play.api.mvc.Request

  implicit def IasI[I]: Mapping[String, I, I] = Success(_)

  implicit def seqAsO[O](implicit m: Mapping[String, String, O]): Mapping[String, Seq[String], O] =
    _.headOption.map(Success[String, String](_)).getOrElse(Failure[String, String](Seq("validation.required"))).flatMap(m)

  implicit def seqAsSeq[O](implicit m: Mapping[String, String, O]): Mapping[String, Seq[String], Seq[O]] =
    data => Validation.sequence(data.map(m))

  implicit def stringAsInt: Mapping[String, String, Int] =
    Constraints.validateWith("validation.type-mismatch"){ (_: String).matches("-?[0-9]+") }(_).map(_.toInt)

  import play.api.libs.json.{ KeyPathNode => JSKeyPathNode, IdxPathNode => JIdxPathNode, _ }
  private def pathToJsPath(p: Path[JsValue]) =
    play.api.libs.json.JsPath(p.path.map{
      case KeyPathNode(key) => JSKeyPathNode(key)
      case IdxPathNode(i) => JIdxPathNode(i)
    })

  implicit def jsonAsString: Mapping[String, JsValue, String] = {
    case JsString(v) => Success(v)
    case _ => Failure(Seq("validation.type-mismatch"))
  }

  implicit def jsonAsInt: Mapping[String, JsValue, Int] = {
    case JsNumber(v) => Success(v.toInt) // XXX
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

  type M = Map[String, Seq[String]]

  private def toMapKey(p: Path[M]) = p.path.head.toString + p.path.tail.foldLeft("") {
    case (path, IdxPathNode(i)) => path + s"[$i]"
    case (path, KeyPathNode(k)) => path + "." + k
  }

  implicit def pickInMap[O](p: Path[M])(implicit m: Mapping[String, Seq[String], O]): Mapping[String, M, O] = {
    data =>
      val key = toMapKey(p)
      val validation: Validation[String, Seq[String]] =
        data.get(key).map(Success[String, Seq[String]](_)).getOrElse{ Failure[String, Seq[String]](Seq("validation.required")) }
      validation.flatMap(m)
  }

  implicit def pickSInMap[O](p: Path[M])(implicit m: Mapping[String, String, O]): Mapping[String, M, Seq[O]] = { data =>
    val prefix = toMapKey(p)
    val r = prefix + """\[([0-9]+)\]"""

    // TODO: DRY
    val ss: Seq[String] = data.filterKeys(_.matches(r)).groupBy { case (k, v) =>
      val r.r(index) = k
      index.toInt
    }.toSeq.sortBy(_._1)
    .flatMap( _._2.toSeq.map{ case (k, v) => v }).flatten
    Validation.sequence(ss.map(m))
  }

  implicit def mapPickMap(p: Path[M]): Mapping[String, M, M] = { data =>
    val prefix = toMapKey(p) + "."
    val submap = data.filterKeys(_.startsWith(prefix)).map { case (k, v) =>
      k.substring(prefix.length) -> v
    }
    Success(submap)
  }

  implicit def mapPickSeqMap(p: Path[M]): Mapping[String, M, Seq[M]] = { data =>
    val prefix = toMapKey(p)
    val r = prefix + """\[([0-9]+)\]*\.(.*)"""

    // XXX: ugly and clearly not efficient
    val submaps: Seq[M] = data.filterKeys(_.matches(r)).groupBy { case (k, v) =>
      val r.r(index, name) = k
      index.toInt
    }.toSeq.sortBy(_._1).map(_._2).map( _.map{ case (k, v) =>
        val r.r(index, name) = k
        name -> v
    })
    Success(submaps)
  }

}

object Constraints {
  import scala.util.matching.Regex

  def validateWith[From](msg: String)(pred: From => Boolean): Constraint[From] =
    v => if(!pred(v)) Failure(Seq(msg)) else Success(v)

  def optional[O](c: Constraint[O]): Constraint[Option[O]] =
    _.map(v => c(v).map(Some.apply)).getOrElse(Success(None))

  //TODO: keep index in path
  def seq[O](c: Constraint[O]): Constraint[Seq[O]] =
    vs => Validation.sequence(vs.map(c))

  def seq[I, O](r: Rule[I, O]): Rule[Seq[I], Seq[O]] = {
    Rule(Path[Seq[I]](), { p => d =>
      val vs = d.map(r.validate)

      val withI = vs.zipWithIndex.map { case (v, i) =>
        v.fail.map { errs =>
          errs.map { case (path, es) => (p.as[Seq[I]] \ i).compose(path.as[Seq[I]]) -> es }
        }
      }

      Validation.sequence(withI)

    }, seq(r.v))
  }

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