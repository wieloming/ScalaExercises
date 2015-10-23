def listFlatten[T](listToFlatten: List[List[T]]): List[T] = {
  var flattenList: List[T] = List.empty
  for (array <- listToFlatten) {
    flattenList = flattenList ++ array
  }
  flattenList
}

def maybeFlatten[T](monad: Option[Option[T]]): Option[T] = {
  monad match {
    case Some(m) => m
    case None => None
  }
}
listFlatten[Int](List(List(1, 2, 4), List(5, 6, 7))).foreach(println)
maybeFlatten[Int](Some(Some(5))).foreach(println)