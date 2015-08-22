trait GenericCategory[->>[_, _]] {
  def ID[A]: A ->> A

  def COMPOSE[A, B, C](g: B ->> C, f: A ->> B): A ->> C
}

object Category extends GenericCategory[Function] {
  def ID[A]: A => A = a => a

  def COMPOSE[A, B, C](g: B => C, f: A => B): A => C =
    g compose f
}

trait Monoid[T] {
  def id: T

  def operation(a: T, b: T): T

  //Monoid homomorphisms
  //(a.length + b.length) == (a + b).length
  //Monoid isomorphisms
  //If f(g(b)) == b and g(f(a)) == a, f and g form an isomorphism.
}

trait Monad[M[_]] {
  def apply[T](a: T): M[T]

  def flatMap[T, U](m: M[T])(f: T => M[U]): M[U]
}
