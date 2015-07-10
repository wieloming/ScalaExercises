type Map[T] = (String) => Option[T]

def createMap[T](key: String, value: T):Map[T] = (k: String) => {
  if(k == key){
    Some(value)
  }else{
    None
  }
}

def concat[T](map1: Map[T], map2: Map[T]) = (key:String) => {
  map1(key) match {
    case Some(x) => Some(x)
    case None => map2(key)
  }
}

val name = createMap("name", "John")
val surname = createMap("surname", "Doe")
val age = createMap("age", "21")

val user = concat(name, concat(surname, age))
user("name").foreach(println)
user("surname").foreach(println)
user("age").foreach(println)
