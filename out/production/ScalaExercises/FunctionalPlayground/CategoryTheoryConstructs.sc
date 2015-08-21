trait GenericCategory[->>[_, _]] {
  def ID[A]: A ->> A

  def COMPOSE[A, B, C](g: B ->> C, f: A ->> B): A ->> C
}

object Category extends GenericCategory[Function] {
  def ID[A]: A => A = a => a

  def COMPOSE[A, B, C](g: B => C, f: A => B): A => C =
    g compose f
}

trait GenericFunctor[->>[_, _], ->>>[_, _], F[_]] {
  def MAP[A, B](f: A ->> B): F[A] ->>> F[B]
}

trait Functor[F[_]] extends GenericFunctor[Function, Function, F] {
  final def MAP[A, B](as: F[A])(f: A => B): F[B] =
    MAP(f)(as)
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

trait ApplicativeFunctor[F[_]] extends Functor[F] {
  def lift[A](a: A): F[A]

  def zip[A, B](a: F[A], b: F[B]): F[(A, B)]
}

trait Traverser[Collection[_]] {
  def traverse[A, B, F[_]](
                            value: Collection[A],
                            operation: A => F[B],
                            ctx: ApplicativeFunctor[F]
                            ): F[Collection[B]]
}

trait Monoid[T] {
  def id: T

  def operation(a: T, b: T): T
}

trait Monad[M[_]] {
  def apply[T](a: T): M[T]

  def flatMap[T, U](m: M[T])(f: T => M[U]): M[U]
}

