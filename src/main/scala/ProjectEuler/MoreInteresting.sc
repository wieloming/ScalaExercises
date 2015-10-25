//Fibonacci sequence whose values do not exceed four million, find the sum of the even-valued terms.
val fibs: Stream[Int] = 0 #:: fibs.scanLeft(1)(_ + _)
val fibs2: Stream[Int] = 0 #:: 1 #:: fibs2.zip(fibs2.tail).map(t => t._1 + t._2)

fibs
  .filter(_ % 2 == 0)
  .takeWhile(_ < 4000000).sum










//triangularNumbers//
val triangular: Stream[Int] = Stream.from(2).scanLeft(1)(_ + _)
triangular.take(10).force










//find route with biggest sum in:
//75
//95 64
//17 47 82
//18 35 87 10
//20 04 82 47 65
//19 01 23 75 03 34
//88 02 77 73 07 63 67
//99 65 04 28 06 16 70 92
//41 41 26 56 83 40 80 70 33
//41 48 72 33 47 32 37 16 94 29
//53 71 44 65 25 43 91 52 97 51 14
//70 11 33 28 77 73 17 78 39 68 17 57
//91 71 52 38 17 14 91 43 58 50 27 29 48
//63 66 04 68 89 53 67 30 73 16 69 87 40 31
//04 62 98 27 23 09 70 98 73 93 38 53 60 04 23
val triangle = "75\n95 64\n17 47 82\n18 35 87 10\n20 04 82 47 65\n19 01 23 75 03 34\n88 02 77 73 07 63 67\n99 65 04 28 06 16 70 92\n41 41 26 56 83 40 80 70 33\n41 48 72 33 47 32 37 16 94 29\n53 71 44 65 25 43 91 52 97 51 14\n70 11 33 28 77 73 17 78 39 68 17 57\n91 71 52 38 17 14 91 43 58 50 27 29 48\n63 66 04 68 89 53 67 30 73 16 69 87 40 31\n04 62 98 27 23 09 70 98 73 93 38 53 60 04 23"
val table = triangle
  .split("\n")
  .toList
  .map(_.split(" ").toList.map(_.toInt))
def traverse(table: List[List[Int]], currentIndex: (Int, Int), sum: Int, result: List[Int]): List[Int] ={
  val (y, x) = currentIndex
  if(y == table.length) sum :: result
  else {
    val currentValue = table(y)(x)
    val res1 = traverse(table, (y + 1, x), sum + currentValue, result)
    val res2 = traverse(table, (y + 1, x+1), sum + currentValue, result)
    result ++ res1 ++ res2
  }
}
traverse(table, (0, 0), 0, List.empty).max
