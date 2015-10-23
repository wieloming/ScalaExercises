package CategoryTheoryConstructs

//  like functors, but functions are now also wraped in boxes
trait ApplicativeFunctor[F[_]] extends Functor[F] {
  def PURE[A](a: A): F[A]
  //similar to map in Functor(only ff is in monad)
  def APPLY[A, B](fa: F[A])(ff: F[A => B]): F[B]

  //map defined in terms of pure and apply
  override def MAP[A, B](fa: F[A])(f: A => B): F[B] = APPLY(fa)(PURE(f))

  def APPLY2[A, B, Z](fa: F[A], fb: F[B])(fabz: F[(A, B) => Z]) = {
    APPLY(fa)(APPLY(fb)(MAP(fabz)(f => b => a => f(a, b))))
  }

  def MAP2[A, B, Z](fa: F[A], fb: F[B])(f: (A, B) => Z): F[Z] = APPLY(fa)(MAP(fb)(b => f(_, b)))

  def TUPPLE[A, B](fa: F[A], fb: F[B]): F[(A, B)] = MAP2(fa, fb)((a, b) => (a, b))

  def FLIP[A, B](ff: F[A => B]): F[A] => F[B] = fa => APPLY(fa)(ff)
}

object ApplicativeFunctor {
    implicit val optionApplicative: ApplicativeFunctor[Option] = new ApplicativeFunctor[Option] {
      def PURE[A](a: A): Option[A] = Some(a)
      def APPLY[A, B](fa: Option[A])(ff: Option[A => B]): Option[B] = (fa, ff) match {
        case (None, _) => None
        case (_, None) => None
        case (Some(a), Some(f)) => Some(f(a))
      }
    }
    implicit val listApplicative: ApplicativeFunctor[List] = new ApplicativeFunctor[List] {
      def PURE[A](a: A): List[A] = List(a)
      def APPLY[A, B](fa: List[A])(ff: List[A => B]): List[B] =
        for {
          a <- fa
          f <- ff
        } yield f(a)
    }
    implicit val streamApplicative: ApplicativeFunctor[Stream] = new ApplicativeFunctor[Stream] {
      //continually ads elements to stream...
      override def PURE[A](a: A): Stream[A] = Stream.continually(a)
      override def APPLY[A, B](fa: Stream[A])(ff: Stream[(A) => B]): Stream[B] =
        (fa zip ff).map({case (a, f) => f(a)})

    }
}
