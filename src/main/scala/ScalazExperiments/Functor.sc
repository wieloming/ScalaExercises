import scalaz.Scalaz._

// map on tuples
(1, 2, 3).map(_ + 1)
// map on functions
((x: Int) => x + 1).map(_ * 7)

List(1, 2, 3) as "x"
List(1, 2, 3).void

List(1, 2, 3).strengthL("x")
List(1, 2, 3).strengthR("x")