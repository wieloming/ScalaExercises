type IteratingFunct = String => Unit
type List = IteratingFunct => Unit

def NIL: List = (f: IteratingFunct) => {}
def CONS(el: String, list: List): List = (f: IteratingFunct) => {
  f(el)
  list(f)
}
def HEAD(list: List): String = {
  var counter = 0
  var result = ""
  list((el) => {
    if (counter == 0) result = el
    counter += 1
  })
  result
}
def TAIL(list: List): List = {
  var counter = 0
  var result: List = NIL
  list((el) => {
    if (counter > 0) result = CONS(el, result)
    counter += 1
  })
  FLIP(result)
}
def FLIP(list: List): List = {
  var result: List = NIL
  list((el) => {
    result = CONS(el, result)
  })
  result
}
def LENGTH(list: List): Int = {
  var counter = 0
  list((el) => counter += 1)
  counter
}
def FOLDLEFT[T](f: (T, String) => T, sum: T, list: List): T = {
  if (LENGTH(list) > 0) {
    return FOLDLEFT(f, f(sum, HEAD(list)), TAIL(list))
  }
  sum
}
def MAP(list: List, f: (String) => String) = {
  val result = FOLDLEFT[List]((sum: List, el) => CONS(f(el), sum), NIL, list)
  FLIP(result)
}
def FILTER(list: List, f: Any=>Boolean) = {
  val result = FOLDLEFT[List]((sum:List, el) => {
    if(f(el)) CONS(el, sum)
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
println(FOLDLEFT[String]((el, sum) => el + sum, "", list))
println("MAP")
MAP(list, (el) => {el.toUpperCase})(println)
println("FILTER")
FILTER(list, el => el != "b")(println)