sealed trait OPTION

case class SOME(value: Any) extends OPTION
case class NONE(value: Any = null) extends OPTION

def Option(value: Any): OPTION = value match {
  case null => NONE()
  case _ => SOME(value)
}
def getOrElse[T](option: OPTION, el: T) = option match {
  case SOME(x) => x
  case NONE(_) => el
}
def liftToOption[T](f: Any => T) = (el: Any) => Option(f(el))

val n = Option(null)
val s = Option(12)

println("Option(null): " + getOrElse(n, 7))
println("Option(12): " + getOrElse(s, 7))