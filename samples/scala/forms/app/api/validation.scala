package play.api.data.validation2

object Validations {
  type Mapping[Err, From, To] = (From => Validation[Err, To])
  type Constraint[T] = Mapping[String, T, T]
  type VA[To] = Validation[Seq[(Path, Seq[String])], To]
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

  //type V[A] = Validation[String, A]
  //implicit val applicativeV: Applicative[V] = new Applicative[V] {
  //
  //  def pure[A](a:A): V[A] = Success(a)
  //
  //  def map[A, B](m: V[A], f: A => B): V[B] = m.map(f)
  //
  //  def apply[A, B](mf:V[A => B], ma: V[A]): V[B] = (mf, ma) match {
  //    case (Success(f), Success(a)) => Success(f(a))
  //    case (Failure(e1), Failure(e2)) => Failure(e1 ++ e2)
  //    case (Failure(e), _) => Failure(e)
  //    case (_, Failure(e)) => Failure(e)
  //  }
  //}

  // uuuuhhhhh... nasty
  implicit def applicativeFA[From] = new Applicative[({type f[To] = From => VA[To]})#f] {
    override def pure[A](a: A): ({type f[To] = From => VA[To]})#f[A] =
      _ => Success(a)
    override def map[A, B](m: ({type f[To] = From => VA[To]})#f[A], f: A => B): ({type f[To] = From => VA[To]})#f[B] =
      d => m(d).map(f)
    override def apply[A, B](mf: ({type f[To] = From => VA[To]})#f[A => B], ma: ({type f[To] = From => VA[To]})#f[A]): ({type f[To] = From => VA[To]})#f[B] =
      d => ma(d).flatMap{ a => mf(d).map(_(a)) }
  }

  // testing stuff
  //val ap: Applicative[({type f[To] = Map[String, Seq[String]] => VA[To]})#f] = applicativeFA[Map[String, Seq[String]]]
  implicit def cb[To] = play.api.libs.functional.syntax.functionalCanBuildApplicative[({type f[To] = Map[String, Seq[String]] => VA[To]})#f]
  //def cb2[To] = play.api.libs.functional.syntax.functionalCanBuildApplicative[({type f[To] = Map[String, Seq[String]] => VA[To]})#f](ap)
  //def cb3[To] = play.api.libs.functional.syntax.functionalCanBuildApplicative(ap) // Does not work

  implicit def monoidConstraint[T] = new Monoid[Constraint[T]] {
    def append(c1: Constraint[T], c2: Constraint[T]) = v => c1(v) *> (c2(v))
    def identity = Constraints.noConstraint[T]
  }
}