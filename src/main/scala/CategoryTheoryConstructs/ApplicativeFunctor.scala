package CategoryTheoryConstructs

//  like functors, but functions are now also wrapped in boxes
trait ApplicativeFunctor[F[_]] extends Functor[F] {
  //start with those two, because rest can be defined with it
  def PURE[A](a: A): F[A]
  //similar to map in Functor(only ff is in monad)
  def APPLY[A, B](fa: F[A])(ff: F[A => B]): F[B]

  //map defined in terms of pure and apply
  override def MAP[A, B](fa: F[A])(f: A => B): F[B] = APPLY(fa)(PURE(f))

  def APPLY2[A, B, Z](fa: F[A], fb: F[B])(fabz: F[(A, B) => Z]) = {
    APPLY[A, Z](fa)(APPLY(fb)(MAP(fabz)(f => b => a => f(a, b))))
  }

  def MAP2[A, B, Z](fa: F[A], fb: F[B])(f: (A, B) => Z): F[Z] = APPLY(fa)(MAP(fb)(b => f(_, b)))

  def TUPLE[A, B](fa: F[A], fb: F[B]): F[(A, B)] = MAP2(fa, fb)(identity(_,_))

  //similar to lift on functor
  def FLIP[A, B](ff: F[A => B]): F[A] => F[B] = fa => APPLY(fa)(ff)
}

trait ApplicativeLaws {
  def identity[F[_], A](fa: F[A])(implicit F: ApplicativeFunctor[F]) =
    F.APPLY[A, A](fa)(F.PURE(a => a)) == fa
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
      //can be done: def APPLY[A, B](fa: List[A])(ff: List[(A) => B]): List[B] =
      //(fa zip ff).map({case (a, f) => f(a)}) but now APPLY is not working properly
      //if we have one method, and many arguments, it will be done only for first
      def APPLY[A, B](fa: List[A])(ff: List[A => B]): List[B] =
        for {
          a <- fa
          f <- ff
        } yield f(a)
    }
    implicit val streamApplicative: ApplicativeFunctor[Stream] = new ApplicativeFunctor[Stream] {
      //continually ads elements to stream...
      //can be done: PURE[A](a: A): Stream[A] = Stream(a) but now APPLY is not working properly
      //if we have one method, and many arguments, it will be done only for first
      //but with continually every time we ask for a we got it
      override def PURE[A](a: A): Stream[A] = Stream.continually(a)
      override def APPLY[A, B](fa: Stream[A])(ff: Stream[(A) => B]): Stream[B] =
        (fa zip ff).map({case (a, f) => f(a)})

    }
}
