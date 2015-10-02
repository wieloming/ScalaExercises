package CategoryTheoryConstructs

trait Semigroup[A]{
  def combine(x: A, y: A): A
}

trait SemigroupLaws[A] {
  def asociative(x: A, y:A, z: A)(implicit S: Semigroup[A]) =
    S.combine(S.combine(x, y), z) == S.combine(S.combine(y, z), x)
}