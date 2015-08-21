type MyList = String => Any

def NIL: MyList = (in: String) => false

def CONS(head: Any, tail: Any): MyList =
  (in: String) => in.toUpperCase match {
    case "HEAD" => head
    case "TAIL" => tail
    case _ => throw new Exception("only head and tail accepted")
  }

def FOLDLEFT(f: (Any, Any) => Any, sum: Any, list: Any): Any =
  list match {
    case list:MyList if list("head") == false => sum
    case list:MyList => FOLDLEFT(f, f(sum, list("head")), list("tail"))
    case _ =>
  }

def MAP(f: Any => Any, list: MyList):MyList = {
  FOLDLEFT((acc, el) => CONS(f(el), acc), NIL, list).asInstanceOf[MyList]
}

val myList = CONS("a", CONS("b", CONS("c", NIL)))

FOLDLEFT((x:Any, y:Any) => (x, y), "ooo", myList)

MAP((el) => println(el + ","), myList)