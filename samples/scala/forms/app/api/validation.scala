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

  // TODO: rename
  def *>[B](o: Validation[E, B]): Validation[E, B] = (this, o) match {
    case (Success(_), Success(v)) => Success(v)
    case (Success(_), Failure(e)) => Failure(e)
    case (Failure(e), Success(_)) => Failure(e)
    case (Failure(e1), Failure(e2)) => Failure(e1 ++ e2)
  }

  def fail = FailProjection(this)
  def success = SuccessProjection(this)
}

case class FailProjection[E, +A](v: Validation[E, A]) {
  def map[F](f: Seq[E] => Seq[F]): Validation[F, A] = v match {
    case Success(v) => Success(v)
    case Failure(e) => Failure(f(e))
  }
}
case class SuccessProjection[E, +A](v: Validation[E, A]) {
  def map[B](f: A => B): Validation[E, B] = v match {
    case Success(v) => Success(f(v))
    case Failure(e) => Failure(e)
  }
}

final case class Success[E, A](a: A) extends Validation[E, A]
final case class Failure[E, A](errors: Seq[E]) extends Validation[E, A]

object Failure {
  import play.api.libs.functional.Monoid

  def merge[E, A](e1: Failure[E, A], e2: Failure[E, A]): Failure[E, A] = {
    Failure(e1.errors ++ e2.errors)
  }
}


object syntax {

  import Validations._
  import play.api.libs.functional._

  type VA[A] = Validation[String, A]
  implicit val applicativeVA: Applicative[VA] = new Applicative[VA] {

    def pure[A](a:A): VA[A] = Success(a)

    def map[A, B](m: VA[A], f: A => B): VA[B] = m.map(f)

    def apply[A, B](mf:VA[A => B], ma: VA[A]): VA[B] = (mf, ma) match {
      case (Success(f), Success(a)) => Success(f(a))
      case (Failure(e1), Failure(e2)) => Failure(e1 ++ e2)
      case (Failure(e), _) => Failure(e)
      case (_, Failure(e)) => Failure(e)
    }
  }

/*
  implicit val applicativeExtractor = new Applicative[Extractor] {
    def pure[A](a: A) = new Extractor[A] {
      // XXX: this implicit does not make any kind of sense
      def apply[S](data: S)(implicit m: F[S]) = Success(a)
    }

    def map[A, B](e: Extractor[A], f: A => B) = new Extractor[B] {
      def apply[S](data: S)(implicit m: F[S]) = ???
    }

    def apply[A, B](mf: Extractor[A => B], ma: Extractor[A]) = ???
  }
*/

  implicit def monoidConstraint[T] = new Monoid[Constraint[T]] {
    def append(c1: Constraint[T], c2: Constraint[T]) = v => c1(v) *> (c2(v))
    def identity = Constraints.noConstraint[T]
  }
}