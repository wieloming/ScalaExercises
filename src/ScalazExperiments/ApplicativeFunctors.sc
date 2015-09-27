//import scala.util.Try
//import scalaz._
//
//case class Foo(s: Symbol, n: Int)
//
//val whatever = 4
//
//def maybeComputeS (i: Int) = Try (Symbol (i.toString) )
//
//def maybeComputeN (i: Int) = Try (i)
//
//val maybeFoo = for {
//s <- maybeComputeS (whatever)
//n <- maybeComputeN (whatever)
//} yield Foo (s, n)
//
//val maybeFoo2 = (maybeComputeS (whatever) |@| maybeComputeN (whatever) ) (Foo (_, _) )