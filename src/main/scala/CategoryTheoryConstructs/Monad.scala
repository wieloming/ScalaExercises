package CategoryTheoryConstructs

//Maybe is a Functor, an Applicative, and a Monad
trait Monad[F[_]] extends ApplicativeFunctor[F] {
  override def PURE[A](a: A): F[A]
  def FLATMAP[A, B](m: F[A])(f: A => F[B]): F[B]

  override def MAP[A, B](fa: F[A])(f: A => B): F[B] = FLATMAP(fa)(a => PURE(f(a)))
  override def APPLY[A, B](fa: F[A])(ff: F[A => B]): F[B] = FLATMAP(ff)((f: A => B) => MAP(fa)(f))
  def FLATTEN[A](ffa: F[F[A]]): F[A] = FLATMAP(ffa)(identity)
}

trait MonadLaws[F[_]] {
  this: Monad[F] =>

    def flatMapAssociativity[A, B, C](fa: F[A], afb: A => F[B], bfc: B => F[C]) =
      FLATMAP(FLATMAP(fa)(afb))(bfc) == FLATMAP(FLATMAP(fa)(a => afb(a)))(b => bfc(b))

    def leftIdentity[A, B](a: A, f: A => F[B]): Boolean =
      FLATMAP[A, B](PURE(a))(f) == f(a)

//    def rightIdentity[F[_], A, B](fa: F[B]) = {
//      fa.FLATMAP(a => PURE(a)) == fa
//    }
}

object Monad {
  implicit val listMonad: Monad[List] = new Monad[List] {
    override def PURE[A](a: A): List[A] = List(a)
    override def FLATMAP[A, B](m: List[A])(f: (A) => List[B]): List[B] = m.flatMap(f)
  }
  implicit val optionMonad: Monad[Option] = new Monad[Option] {
    override def PURE[A](a: A): Option[A] = Option(a)
    override def FLATMAP[A, B](m: Option[A])(f: (A) => Option[B]): Option[B] = m.flatMap(f)
  }
}