def paraQuickSort(list: List[Int]): List[Int] = list match {
  case Nil => Nil
  case head :: tail =>
    val smaller = tail.filter(_ < head)
    val larger = tail.filter(_ >= head)
    paraQuickSort(smaller) ++ List(head) ++ paraQuickSort(larger)
}

paraQuickSort(List(0, 8, 9, 7, 5, 6, 4, 3, 1, 2))