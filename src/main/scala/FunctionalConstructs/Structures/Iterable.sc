type IteratingFunct[T] = T => Unit
type List[T] = IteratingFunct[T] => Unit

def NIL[T]: List[T] = (f: IteratingFunct[T]) => {}
def CONS[T](el: T, list: List[T]): List[T] = (f: IteratingFunct[T]) => {
  f(el)
  list(f)
}
def HEAD[T](list: List[T]): T = {
  var counter = 0
  list((el) => {
    if (counter == 0) return el
    counter += 1
  })
  throw new Exception
}
def TAIL[T](list: List[T]): List[T] = {
  var counter = 0
  var result: List[T] = NIL
  list((el) => {
    if (counter > 0) result = CONS(el, result)
    counter += 1
  })
  FLIP(result)
}
def FLIP[T](list: List[T]): List[T] = {
  var result: List[T] = NIL
  list((el) => {
    result = CONS(el, result)
  })
  result
}
def LENGTH[T](list: List[T]): Int = {
  var counter = 0
  list((el) => counter += 1)
  counter
}
def FOLDLEFT[T, G](f: (G, T) => G, sum: G, list: List[T]): G = {
  if (LENGTH(list) > 0) {
    return FOLDLEFT(f, f(sum, HEAD(list)), TAIL(list))
  }
  sum
}
def MAP[T](list: List[T], f: (T) => T) = {
  val result = FOLDLEFT((sum: List[T], el: T) => CONS(f(el), sum), NIL, list)
  FLIP(result)
}
def FILTER[T](list: List[T], f: Any => Boolean) = {
  val result = FOLDLEFT((sum: List[T], el: T) => {
    if (f(el)) CONS(el, sum)
    else sum
  }, NIL, list)
  FLIP(result)
}

val list = CONS("a", CONS("b", CONS("c", NIL)))
println("LISTA")
list(println)
println("HEAD")
println(HEAD(list))
println("TAIL")
TAIL(list)(println)
println("LENGTH")
println(LENGTH(list))
println("FOLDLEFT")
println(FOLDLEFT[String, String]((el, sum) => el + sum, "", list))
println("MAP")
MAP[String](list, (el) => {
  el.toUpperCase
})(println)
println("FILTER")
FILTER(list, el => el != "b")(println)