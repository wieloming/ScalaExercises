package CategoryTheoryConstructs

//for filter on futures
trait Monoid[T] extends Semigroup[T] {
  override def combine(a1: T, a2: T): T

  def id: T
}

trait MonoidLaws {
  //f(a) + f(b) == f(a + b)
  //a.length + b.length == (a + b).length
  def homomorphism[T](a: T, b: T, f: T => T)(implicit M: Monoid[T]) =
    f(M.combine(a, b)) == M.combine(f(a), f(b))
}

object Monoid {
  implicit val IntMonoid: Monoid[Int] = new Monoid[Int] {
    def combine(a: Int, b: Int): Int = a + b
    def id: Int = 0
  }
}

object Test {
  def plus[A: Monoid](a: A, b: A): A =
    implicitly[Monoid[A]].combine(a, b)

  plus(2, 3)

  ////////////////////////////////////////

  trait MonoidOp[A] {
    val F: Monoid[A]
    val value: A
    def |+|(a2: A) = F.combine(value, a2)
  }

  implicit def toMonoidOp[A: Monoid](a: A): MonoidOp[A] = new MonoidOp[A] {
    val F = implicitly[Monoid[A]]
    val value = a
  }

  3 |+| 4
}