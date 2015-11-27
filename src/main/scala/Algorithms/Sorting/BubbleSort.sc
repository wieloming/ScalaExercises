def sort(array: Array[Int]): Array[Int] = {
  for (i <- 0 until array.length - 1; j <- 0 until array.length - 1 - i) {
    if (array(j) > array(j + 1)) {
      val tmp = array(j)
      array(j) = array(j + 1)
      array(j + 1) = tmp
    }
  }
  array
}

def sortRecursive(list: List[Int]): List[Int] = list match {
  case Nil => Nil
  case head :: Nil => List(head)
  case head :: second :: tail if head > second =>
    sortRecursive(List(second, head) ++ tail)
  case head :: tail =>
    val tailSorted = sortRecursive(tail)
    if (head > tailSorted.head)
      sortRecursive(List(tailSorted.head, head) ++ tailSorted.tail)
    else
      List(head) ::: tailSorted
}

sort(Array(7, 6, 4, 5, 3, 1, 2))
sortRecursive(List(7, 6, 4, 5, 3, 1, 2))
