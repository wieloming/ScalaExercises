package CategoryTheoryConstructs

trait GenericFunctor[->>[_, _], ->>>[_, _], F[_]] {
  def MAP[A, B](f: A ->> B): F[A] ->>> F[B]
}

trait Functor[F[_]] extends GenericFunctor[Function, Function, F] {
  def MAP[A, B](as: F[A])(f: A => B): F[B] =
    MAP(f)(as)

  def LIFT[A, B](f: A => B): F[A] => F[B] = {
    fa => MAP(fa)(f)
  }

  def AS[A, B](fa: F[A], b: => B): F[B] = {
    MAP(fa)(_ => b)
  }

  def VOID[A](fa: F[A]): F[Unit] = {
    AS(fa, ())
  }
}

trait FunctorLaws {
  def identity[F[_], A](fa: F[A])(implicit F: Functor[F]) = F.MAP(fa)(a => a) == fa

  def composition[F[_], A, B, C](fa: F[A], f: A => B, g: B => C)(implicit F: Functor[F]) = {
    F.MAP(F.MAP(fa)(f))(g) == F.MAP(fa)(f andThen g)
  }
}

object Functor {
  def MAP[A, B, F[_]](as: F[A])(f: A => B)(implicit functor: Functor[F]): F[B] =
    functor.MAP(as)(f)

  implicit object ListFunctor extends Functor[List] {
    def MAP[A, B](f: A => B): List[A] => List[B] =
      as => as map f
  }

  implicit object OptionFunctor extends Functor[Option] {
    def MAP[A, B](f: A => B): Option[A] => Option[B] =
      o => o map f
  }

  implicit object Function0Functor extends Functor[Function0] {
    def MAP[A, B](f: A => B): (() => A) => (() => B) =
      a => () => f(a())
  }

  MAP(List(1, 2, 3))((x: Int) => x + 1)
  val f = (s: String) => s.length
  val lifted = MAP(() => "abc")(f)
}
