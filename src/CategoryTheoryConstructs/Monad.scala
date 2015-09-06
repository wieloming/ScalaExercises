package CategoryTheoryConstructs

trait Monad[F[_]] extends ApplicativeFunctor[F] {

  override def PURE[A](a: A): F[A]

  def FLATMAP[A, B](m: F[A])(f: A => F[B]): F[B]

  override def MAP[A, B](fa: F[A])(f: A => B): F[B] = {
    FLATMAP(fa)(a => PURE(f(a)))
  }

  override def APPLY[A, B](fa: F[A])(ff: F[A => B]): F[B] = {
    FLATMAP(ff)((f: A => B) => MAP(fa)(f))
  }

  def FLATTEN[A](ffa: F[F[A]]): F[A] = {
    FLATMAP(ffa)(fa => fa)
  }
}

trait MonadLaws[F[_]] {

}

object MonadLaws {
  def APPLY[F[_]](implicit f0: Monad[F]): MonadLaws[F] = new MonadLaws[F] {
    def f = f0
  }
}