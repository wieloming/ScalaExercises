import scalaz.Scalaz._

case class Foo(s: Symbol, n: Int)

val whatever = 4

def maybeComputeS (i: Int) = Option(Symbol (i.toString) )
def maybeComputeN (i: Int) = Option(i)

val maybeFoo = for {
  s <- maybeComputeS (whatever)
  n <- maybeComputeN (whatever)
} yield Foo (s, n)


val maybeFoo2 = (maybeComputeS(whatever) |@| maybeComputeN(whatever) ) (Foo)