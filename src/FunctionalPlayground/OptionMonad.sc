abstract class OPTION[+T]

case class SOME[+T](value: T) extends OPTION
case class NONE[+T](value: T = null) extends OPTION

def Option[T](value: T): OPTION[T] = value match {
  case null => NONE.asInstanceOf[OPTION[T]]
  case _ => SOME(value).asInstanceOf[OPTION[T]]
}

def getOrElse[T](option: OPTION[T], el: T) = option match {
  case SOME(x) => x
  case NONE(x) =>  el
}

def liftToOption[T](f: Any => T) = (el: Any) => Option(f(el))

val n = Option(null)
val s = Option(12)
println(getOrElse(n, 7))
println(getOrElse(s, 7))