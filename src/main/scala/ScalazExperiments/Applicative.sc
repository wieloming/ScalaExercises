import scalaz.Scalaz._

//Composing
case class Foo(s: Symbol, n: Int)
val whatever = 4
def maybeComputeS (i: Int) = Option(Symbol (i.toString))
def maybeComputeN (i: Int) = Option(i)

val maybeFoo = for {
  s <- maybeComputeS (whatever)
  n <- maybeComputeN (whatever)
} yield Foo (s, n)

val maybeFoo2 = (maybeComputeS(whatever) |@| maybeComputeN(whatever) ) (Foo)

// pure:
1.point[Option]
// apply
9.some <*> {x:Int => x + 3}.some
// map2 ??
(3.some |@| 5.some)(_ + _)
(List("ha", "heh", "hmm") |@| List("?", "!", "."))(_ + _)

//////////////////////////////
^(List(1, 2, 3), List(10, 100, 100))(_ * _)
(3.some |@| 5.some)(_ + _)