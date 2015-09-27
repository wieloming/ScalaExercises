def printAll(list: List[Int], num: Int): List[(Int, Int)] = {
  list
    .map(n => (n,list.find(_ + n == num)))
    .filterNot(_._2.isEmpty)
    .map({case (a, Some(b)) => (a, b)})
    .filterNot(t => t._1 > t._2)
    .distinct
}

def printAll2(list: List[Int], num: Int): List[List[Int]] = list match {
  case Nil => Nil
  case head::Nil => Nil
  case head :: tail if head + tail.last == num =>
     List(head, tail.last) :: printAll2(tail.init, num)
  case head :: tail if head + tail.last < num =>
    printAll2(tail, num)
  case head :: tail if head + tail.last > num =>
    printAll2(head :: tail.init, num)
}
printAll(List(1, 2, 3, 4, 6, 6, 6, 9, 9, 10), 12)
printAll2(List(1, 2, 3, 4, 6, 6, 6, 9, 9, 10), 12)
