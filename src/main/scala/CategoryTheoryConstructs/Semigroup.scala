package CategoryTheoryConstructs

//required for fold
trait Semigroup[A]{
  def combine(x: A, y: A): A
}

trait SemigroupLaws[A] {
  // (a + b) + c == (b + c) + a
  def asociative(x: A, y:A, z: A)(implicit S: Semigroup[A]) =
    S.combine(S.combine(x, y), z) == S.combine(S.combine(y, z), x)
}