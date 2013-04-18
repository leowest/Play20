package play.api.data.validation2

import Validations._
import scala.language.existentials

object Form {

  private val __ = Path[Map[String, Seq[String]]]()

  def asKey(p: Path[Map[String, Seq[String]]]) = p.path.head.toString + p.path.tail.foldLeft("") {
    case (path, IdxPathNode(i)) => path + s"[$i]"
    case (path, KeyPathNode(k)) => path + "." + k
  }

  def asPath(key: String) =
    (__ \ key)
}

case class Form[T](data: Map[String, Seq[String]] = Map.empty, errors: Seq[(Path[Map[String,Seq[String]]], Seq[String])] = Nil, value: Option[T] = None) {
  lazy val hasErrors: Boolean = !errors.isEmpty

  def apply(key: String): Field = apply(Form.asPath(key))

  def apply(path: Path[Map[String, Seq[String]]]): Field = {
    val es = errors.flatMap {
      case e @ (p, es) if p == path =>
        es.map(error => FormError(p, error))
      case _ => Nil
    }

    val value = data.get(Form.asKey(path)).flatMap(_.headOption)

    Field(this, path, es, value)
  }
}

case class Field(private val form: Form[_], path: Path[Map[String, Seq[String]]], errors: Seq[FormError], value: Option[String]) {
  lazy val error: Option[FormError] = errors.headOption
  lazy val hasErrors: Boolean = !errors.isEmpty

  def apply(key: String): Field = {
    apply(Form.asPath(key))
  }

  def apply(index: Int): Field = {
    apply(Path[Map[String, Seq[String]]](IdxPathNode(index) :: Nil))
  }

  def apply(_path: Path[Map[String, Seq[String]]]): Field = {
    val prefix = Form.asKey(_path)

    val d = form.data.filter(_._1.startsWith(prefix)).map(_._2).flatten

    val errs: Seq[FormError] = errors.filter(_.path.path.take(_path.path.size) == _path).map{ f =>
      val p = Path[Map[String, Seq[String]]](f.path.path.drop(_path.path.size))
      FormError(p, f.message, f.args)
    }

    Field(form, path.compose(_path), errs, d.headOption)
  }

  def name = Form.asKey(path)

  lazy val indexes: Seq[Int] = {
    Nil // TODO
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