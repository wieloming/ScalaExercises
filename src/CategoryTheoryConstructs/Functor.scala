package CategoryTheoryConstructs

trait GenericFunctor[->>[_, _], F[_]] {
  def MAP[A, B](as: F[A])(f: A ->> B): F[B]
}

trait Functor[F[_]] extends GenericFunctor[Function, F] {self =>
  override def MAP[A, B](as: F[A])(f: A => B): F[B]

  //can be defined on functor
  def LIFT[A, B](f: A => B): F[A] => F[B] = fa => MAP(fa)(f)
  // AS and VOID are structure preserving List(1, 2).AS = List((), ())
  def AS[A, B](fa: F[A], b: => B): F[B] = MAP(fa)(_ => b)
  def VOID[A](fa: F[A]): F[Unit] = AS(fa, ())
//TODO: implement X => F[G[X]]
//  def COMPOSE[G[_]](implicit G: Functor[G]): Functor[{type L[a] = F[G[a]]}] =
//    new Functor[{type L[a] = F[G[a]]}] {
//      def MAP[A, B](fga: F[G[A]])(f: A => B): F[G[B]] = self.MAP(fga)(ga => G.MAP(ga)(a => f(a)))
//    }
}

trait FunctorLaws {
  def identity[F[_], A](fa: F[A])(implicit F: Functor[F]) = F.MAP(fa)(a => a) == fa
  def composition[F[_], A, B, C](fa: F[A], f: A => B, g: B => C)(implicit F: Functor[F]) =
    F.MAP(F.MAP(fa)(f))(g) == F.MAP(fa)(f andThen g) // fa.map(A=>B).map(B=>C) == fa.map(A=>C)
}

object Functor {
  def MAP[A, B, F[_]](as: F[A])(f: A => B)(implicit functor: Functor[F]): F[B] = functor.MAP(as)(f)

  implicit val ListFunctor = new Functor[List] {
    def MAP[A, B](fa: List[A])(f: A => B): List[B] = fa map f
  }
  implicit val OptionFunctor = new Functor[Option] {
    def MAP[A, B](fa: Option[A])(f: A => B): Option[B] = fa map f
  }
  implicit def Function0Functor[X] = new Functor[Function0] {
    override def MAP[A, B](as: () => A)(f: A => B): () => B = () => f(as())
  }
//  (partially aplying types in type constructor)
//   functor mapping on function result(np. .map(_ + 1))
    implicit def Function1Functor[X]: Functor[({type L[a] = (X) => a})#L] = new Functor[({type L[a] = (X) => a})#L] {
      def MAP[A, B](fa: X => A)(f: A => B): X => B =
        fa andThen f
    }
}
