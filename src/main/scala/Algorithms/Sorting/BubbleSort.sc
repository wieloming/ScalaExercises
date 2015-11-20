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

def sortRecursive(list: List[Int]): List[Int] = {
  if (list != Nil && list.tail != Nil) {
    if (list.head > list.tail.head) {
      sortRecursive(List(list.tail.head, list.head) ++ list.tail.tail)
    } else {
      val tailSorted = sortRecursive(list.tail)
      if (list.head > tailSorted.head)
        sortRecursive(List(tailSorted.head, list.head) ++ tailSorted.tail)
      else
        List(list.head) ::: tailSorted
    }
  } else {
    list
  }
}

sort(Array(7, 6, 4, 5, 3, 1, 2))
sortRecursive(List(7, 6, 4, 5, 3, 1, 2))
