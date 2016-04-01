case class Reader[A, B](run: A => B) {
  def map[C](f: B => C): Reader[A, C] = {
    Reader(run andThen f)
  }

  def flatMap[C](f: B => Reader[A, C]): Reader[A, C] = {
    Reader { a =>
      val b = run(a)
      val r: Reader[A, C] = f(b)
      val ac: A => C = r.run
      val c: C = ac(a)
      c
    }
  }
}