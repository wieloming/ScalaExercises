package CategoryTheoryConstructs


trait Monoid[T] {
  def id: T

  def operation(a: T, b: T): T

  //Monoid homomorphisms
  //(a.length + b.length) == (a + b).length
  //Monoid isomorphisms
  //If f(g(b)) == b and g(f(a)) == a, f and g form an isomorphism.
}
