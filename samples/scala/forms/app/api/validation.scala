package play.api.data.validation2

object Validations {
  type Mapping[Err, From, To] = (From => Validation[Err, To])
  type Constraint[T] = Mapping[String, T, T]
  type VA[To] = Validation[(Path, Seq[String]), To]

  import play.api.libs.functional._

  implicit def monoidConstraint[T] = new Monoid[Constraint[T]] {
    def append(c1: Constraint[T], c2: Constraint[T]) = v => c1(v) *> (c2(v))
    def identity = Constraints.noConstraint[T]
  }

  implicit def applicativeRule[I] = new Applicative[({type f[O] = Rule[I, O]})#f] {
    override def pure[A](a: A): Rule[I, A] =
      Rule(Path, (_: Path) => (_: I) => Success(a))

    override def map[A, B](m: Rule[I, A], f: A => B): Rule[I, B] =
      Rule(m.p, { p => d => m.m(p)(d).map(f) })

    // case class Rule[I, O](p: Path, m: Path => Mapping[String, I, O], v: Constraint[O] = Constraints.noConstraint[O]) {
    override def apply[A, B](mf: Rule[I, A => B], ma: Rule[I, A]): Rule[I, B] =
      Rule(ma.p, { p => d =>
        val a = ma.validate(d)
        val f = mf.validate(d)
        a.flatMap(x => f.map(_(x)))
      })
  }

  implicit def functorRule[I] = new Functor[({type f[O] = Rule[I, O]})#f] {
    def fmap[A, B](m: Rule[I, A], f: A => B): Rule[I, B] = applicativeRule[I].map(m, f)
  }

  // Helps the compiler a bit
  import play.api.libs.functional.syntax._
  implicit def cba[I] = functionalCanBuildApplicative[({type f[O] = Rule[I, O]})#f]
  implicit def fbo[I] = toFunctionalBuilderOps[({type f[O] = Rule[I, O]})#f, String] _
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
