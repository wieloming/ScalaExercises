//Fibonacci sequence whose values do not exceed four million, find the sum of the even-valued terms.
val fibs: Stream[Int] = 0 #:: fibs.scanLeft(1)(_ + _)
val fibs2: Stream[Int] = 0 #:: 1 #:: fibs2.zip(fibs2.tail).map(t => t._1 + t._2)
fibs
  .filter(_ % 2 == 0)
  .takeWhile(_ < 4000000).sum

//triangularNumbers
val triangular: Stream[Int] = Stream.from(2).scanLeft(1)(_ + _)
triangular.take(10).force

