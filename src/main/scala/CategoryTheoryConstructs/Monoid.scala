package CategoryTheoryConstructs

trait Monoid[T] extends Semigroup[T] {
  def id: T
}

trait MonoidLaws {
  //f(a) + f(b) == f(a + b)
  //a.length + b.length == (a + b).length
  def homomorphism[T](a: T, b: T, f: T => T)(implicit M: Monoid[T]) =
    f(M.combine(a, b)) == M.combine(f(a), f(b))
}
