import scala.reflect.ClassTag

def arrayFlatten[T: ClassTag](arrayToFlatten: Array[Array[T]]): Array[T] = {
  var flattenArray: Array[T] = Array[T]()
  for (array <- arrayToFlatten) {
    flattenArray = flattenArray ++ array
  }
  flattenArray
}

def maybeFlatten[T](monad: Option[Option[T]]): Option[T] = {
  monad match {
    case Some(m) => m
    case None => None
  }
}
arrayFlatten[Int](Array(Array(1, 2, 4), Array(5, 6, 7))).foreach(println)
maybeFlatten[Int](Some(Some(5))).foreach(println)