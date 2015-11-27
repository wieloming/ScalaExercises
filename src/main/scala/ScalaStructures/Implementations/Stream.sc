case object Empty extends STREAM[Nothing]

case class Cons[+A](h: () => A, t: () => STREAM[A]) extends STREAM[A]

object STREAM {
  def cons[A](hd: => A, tl: => STREAM[A]): STREAM[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def apply[A](as: A*): STREAM[A] = {
    if (as.isEmpty) Empty
    else cons(as.head, apply(as.tail: _*))
  }
}

trait STREAM[+A] {
  def toList: List[A] = this match {
    case Empty => Nil
    case Cons(h, t) => h() :: t().toList
  }
}
STREAM(1, 2, 3, 4)
STREAM(1, 2, 3, 4).toList