def getSubsequences(list: List[Int]): List[List[Int]] = {
  list match {
    case Nil => Nil
    case head :: Nil => List(list)
    case head :: tail =>
      val allForPrevious = getSubsequences(tail)
      allForPrevious ++ (List(head) :: allForPrevious.map(head::_))
  }
}

def checkIfIncreasing(list: List[Int]): Boolean = list match {
  case Nil => true
  case head :: Nil => true
  case head :: tail => head < tail.head && checkIfIncreasing(tail)
}
getSubsequences(List(2, 6, 4, 5, 1, 3))
  .filter(checkIfIncreasing)
  .sortBy(-_.length)
  .headOption
  .foreach(println)
