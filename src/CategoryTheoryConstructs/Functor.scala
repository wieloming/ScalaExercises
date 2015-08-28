package CategoryTheoryConstructs

trait GenericFunctor[->>[_, _], ->>>[_, _], F[_]] {
  def MAP[A, B](f: A ->> B): F[A] ->>> F[B]
}

trait Functor[F[_]] {
  def MAP[A, B](as: F[A])(f: A => B): F[B]

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

  def identity[F[_], A](fa: F[A])(implicit F: Functor[F]) =
    F.MAP(fa)(a => a) == fa

  def composition[F[_], A, B, C](fa: F[A], f: A => B, g: B => C)(implicit F: Functor[F]) = {
    F.MAP(F.MAP(fa)(f))(g) == F.MAP(fa)(f andThen g)
  }
}

object Functor {
  def MAP[A, B, F[_]](as: F[A])(f: A => B)(implicit functor: Functor[F]): F[B] =
    functor.MAP(as)(f)

  implicit val ListFunctor = new Functor[List] {
    def MAP[A, B](fa: List[A])(f: A => B): List[B] =
      fa map f
  }

  implicit val OptionFunctor = new Functor[Option] {
    def MAP[A, B](fa: Option[A])(f: A => B): Option[B] =
      fa map f
  }

  //TODO: how to implement '?' type
//  implicit def Function0Functor[X] = new Functor[X=> ?] {
//    def MAP[A, B](fa: X => A)(f: A => B): X => B =
//      fa andThen f
//  }

  MAP(List(1, 2, 3))((x: Int) => x + 1)
  val f = (s: String) => s.length
  val lifted = MAP(() => "abc")(f)
}
