package play.api.data.validation2

object Validations {
  type Mapping[Err, From, To] = (From => Validation[Err, To])
  type Constraint[T] = Mapping[String, T, T]
  type VA[Key, To] = Validation[Seq[(Key, Seq[String])], To]
}

sealed trait Validation[E, +A] {
  def map[X](f: A => X): Validation[E, X] = this match {
    case Success(v) => Success(f(v))
    case Failure(e) => Failure(e)
  }

  def flatMap[X](f: A => Validation[E, X]): Validation[E, X] = this match {
    case Success(v) => f(v)
    case Failure(e) => Failure(e)
  }

  def fail = ???
}
final case class Success[E, A](a: A) extends Validation[E, A]
final case class Failure[E, A](errors: Seq[E]) extends Validation[E, A]

object Failure {
  import play.api.libs.functional.Monoid

  def merge[E, A](e1: Failure[E, A], e2: Failure[E, A]): Failure[E, A] = {
    Failure(e1.errors ++ e2.errors)
  }
}


object Validation {

  /*
  import play.api.libs.functional._

  implicit def alternativeValidation(implicit a: Applicative[Validation]): Alternative[Validation] = new Alternative[Validation] {
    val app = a
    def |[E1, E2 >: E1, A, B >: A](alt1: Validation[E1, A], alt2 :Validation[E2, B]): Validation[E2, B] = (alt1, alt2) match {
      case (Failure(e), Success(v)) => Success(v)
      case (Success(v), _) => Success(v)
      case (Failure(e1), Failure(e2)) => Failure(Failure.merge(e1, e2))
    }
    def empty: Failure[Nothing, Nothing] = Failure(Seq())
  }

  type VA[A] = Validation[String, A]
  implicit val applicativeVA: Applicative[VA] = new Applicative[VA] {

    def pure[A](a:A): VA[A] = Success(a)

    def map[A, B](m: VA[A], f: A => B): VA[B] = m.map(f)

    def apply[A, B](mf:VA[A => B], ma: VA[A]): VA[B] = (mf, ma) match {
      case (Success(f,_), Success(a,_)) => Success(f(a))
      case (Failure(e1), Failure(e2)) => Failure.merge(e1, e2)
      case (Failure(e), _) => Failure(e)
      case (_, Failure(e)) => Failure(e)
    }
  }
  */
}