object CATEGORY {
  def ID[A]: A => A = a => a
  def COMPOSE[A, B, C](g: B => C, f: A => B): A => C =
    g compose f
}