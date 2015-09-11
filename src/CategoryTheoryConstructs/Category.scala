package CategoryTheoryConstructs

trait GenericCategory[->>[_, _]] {
  def ID[A]: A ->> A
  def COMPOSE[A, B, C](g: B ->> C, f: A ->> B): A ->> C
}

object Category extends GenericCategory[Function] {
  def ID[A]: A => A = a => a
  def COMPOSE[A, B, C](g: B => C, f: A => B): A => C = g compose f
}
