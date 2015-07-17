//trait GenList[T] {
//  def apply(in:String):Either[T, GenList[T]]
//}
//
//type GenList[T] = (String) => [T]
//
//def NIL:GenList[Boolean] = (in: String) => false
//
//def CONS[T, Any](head: T, tail: GenList[T]): GenList[T] =
//  (in: String) => in.toUpperCase match {
//    case "HEAD" => head
//    case "TAIL" => tail
//  }


//trait GenList[T] {
//  def apply(in:String):Either[T, GenList[T]]
//}
//
//type GenList[T] = (String) => T
//
//def NIL[T]:GenList[T] = (in: String) => false
//
//def CONS[T, Any](head: T, tail: GenList[T]): GenList[T] =
//  (in: String) => in.toUpperCase match {
//    case "HEAD" => head
//    case "TAIL" => tail
//  }