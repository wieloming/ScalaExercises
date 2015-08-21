sealed trait List[+A]

case object Nil extends List[Nothing]

case class Cons[+A](head: A, tail: List[A]) extends List[A]

val ex1: List[Double] = Nil
val ex2: List[Int] = Cons(1, Nil)

object List {
  def apply[A](as: A*): List[A] = {
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
  }
}

def tail[A](ds: List[A]) = ds match {
  case Nil => throw new Exception
  case Cons(_, t) => t
}
def init[A](ls: List[A]): List[A] = ls match {
  case Nil => throw new Exception
  case Cons(h, Nil) => Nil
  case Cons(h, t) => Cons(h, init(t))
}

val a = Cons(1, Cons(2, Cons(3, Cons(4, Nil))))
val b = List(1, 2, 3, 4)
tail(b)
init(b)
