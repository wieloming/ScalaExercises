package CategoryTheoryConstructs

trait Monoid[T] {
  def id: T
  def operation(a: T, b: T): T
}

trait MonoidLaws {
  //a.length + b.length == (a + b).length
  def homomorphism[T](a: T, b: T, f: T => T)(implicit M: Monoid[T])= f(M.operation(a, b)) == M.operation(f(a), f(b))

  //f(g(b)) == b and g(f(a)) == a
  // TODO: def isomorphism
}
