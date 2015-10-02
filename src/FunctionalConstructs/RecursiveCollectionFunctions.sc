def reverse[A](list: List[A]): List[A] = list match {
  case Nil => Nil
  case head :: tail => reverse(tail) ++ List(head)
}

reverse(List(1, 2, 3, 4))

def zip[A, B](a: List[A], b: List[B]): List[(A, B)] = (a, b) match {
  case (Nil, _) => Nil
  case (_, Nil) => Nil
  case (ha :: ta, hb :: tb) => (ha, hb) :: zip(ta, tb)
}
zip(List(1, 2, 3, 4), List("A", "B", "C", "D"))