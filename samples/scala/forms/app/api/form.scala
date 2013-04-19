package play.api.data.validation2

import Validations._
import scala.language.existentials

object Form {

  private val __ = Path[Map[String, Seq[String]]]()

  def asKey(p: Path[Map[String, Seq[String]]]) = p.path.head.toString + p.path.tail.foldLeft("") {
    case (path, IdxPathNode(i)) => path + s"[$i]"
    case (path, KeyPathNode(k)) => path + "." + k
  }

  def asPath(k: String) = {
    val Indexed = """^(.*)\[([0-9]+)\]$""".r
    val keys = k.split("\\.").flatMap {
      case Indexed(name, i) =>
        Seq(KeyPathNode(name), IdxPathNode(i.toInt))
      case n =>
        Seq(KeyPathNode(n))
    }
    keys.foldLeft(__)(_ \ _)
  }

  def fill[T](t: T)(implicit w: Writes[T, Map[String, Seq[String]]]) = Form(w.writes(t), Nil, Some(t))
}

case class Form[T](data: Map[String, Seq[String]] = Map.empty, errors: Seq[(Path[Map[String,Seq[String]]], Seq[String])] = Nil, value: Option[T] = None) {
  lazy val hasErrors: Boolean = !errors.isEmpty

  def apply(key: String): Field = apply(Form.asPath(key))

  def apply(path: Path[Map[String, Seq[String]]]): Field = {
    val value = data.get(Form.asKey(path)).flatMap(_.headOption)
    Field(this, path, value)
  }
}

case class Field(private val form: Form[_], path: Path[Map[String, Seq[String]]], value: Option[String]) {
  lazy val error: Option[FormError] = errors.headOption
  lazy val hasErrors: Boolean = !errors.isEmpty

  def apply(key: String): Field = {
    apply(Form.asPath(key))
  }

  def apply(index: Int): Field = {
    apply(Path[Map[String, Seq[String]]]() \ index)
  }

  def errors = form.errors.filter(_._1.path.startsWith(path.path))
    .flatMap { case (p, errs) =>
      errs.map(e => FormError(p, e))
    }

  def apply(_path: Path[Map[String, Seq[String]]]): Field = {
    val expanded = path.compose(_path)
    val prefix = Form.asKey(expanded)
    val d = form.data.filter(_._1.startsWith(prefix)).map(_._2).flatten
    Field(form, expanded, d.headOption)
  }

  def name = Form.asKey(path)

  lazy val indexes: Seq[Int] = {
    form.data.keys.map(Form.asPath)
      .filter(_.path.startsWith(path.path))
      .map(_.path.drop(path.path.length).head)
      .flatMap{
        case IdxPathNode(i) => Seq(i)
        case _ => Seq()
      }.toSeq
  }
}

case class FormError(path: Path[Map[String, Seq[String]]], message: String, args: Seq[Any] = Nil) {
  def withMessage(message: String): FormError = FormError(path, message)
}

object repeat {
  def apply(field: Field, min: Int = 1)(f: Field => play.api.templates.Html) = {
    (0 until math.max(if (field.indexes.isEmpty) 0 else field.indexes.max + 1, min)).map(i => f(field(i)))
  }
}