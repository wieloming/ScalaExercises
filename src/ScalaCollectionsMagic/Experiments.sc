// Split text to lists
val s = "Hello sweet world"
s.split(" ")
  .map(List(_))
  .scan(List.empty)((a, b) => a ++ b)

// check if elements of l1 smaller then l2
val l1 = List(1, 2, 3)
val l2 = List(2, 3, 4)
l1.view zip l2 forall (el => el._1 < el._2)
(l1 corresponds l2)(_ < _)

// equality between data structures
List(1, 2, 3) == Vector(1, 2, 3)
List(1, 2, 3) == (1 to 3)

//triangularNumbers
val triangular: Stream[Int] = Stream.from(2).scanLeft(1)(_ + _)
triangular.take(10).force



















































