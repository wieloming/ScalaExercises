def veryLongFunction(id: Int): Option[String] = {
  Thread.sleep(1000)
  println("looooong time")
  Map(42 -> "result").get(id)
}

def memoizeVeryLongFunction() = {
  var cache = Map[Int, Option[String]]()
  (id: Int) =>
    cache.get(id) match {
      case Some(result) => result
      case None =>
        val result = veryLongFunction(id)
        cache += id -> result
        result
    }
}

val memoize = memoizeVeryLongFunction()

println("first time")
memoize(42)
println("second time")
memoize(42)