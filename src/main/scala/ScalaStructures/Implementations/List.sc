sealed trait LIST[+A]
case object NIL extends LIST[Nothing]
case class CONS[+A](head: A, tail: LIST[A]) extends LIST[A]

val ex1: LIST[Double] = NIL
val ex2: LIST[Int] = CONS(1, NIL)

object LIST {
  def apply[A](as: A*): LIST[A] = {
    if (as.isEmpty) NIL
    else CONS(as.head, apply(as.tail: _*))
  }
}

def tail[A](ds: LIST[A]) = ds match {
  case NIL => throw new Exception
  case CONS(_, t) => t
}
def init[A](ls: LIST[A]): LIST[A] = ls match {
  case NIL => throw new Exception
  case CONS(h, NIL) => NIL
  case CONS(h, t) => CONS(h, init(t))
}

val a = CONS(1, CONS(2, CONS(3, CONS(4, NIL))))
val b = LIST(1, 2, 3, 4)
tail(b)
init(b)
