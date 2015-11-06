type NUMBER = (Any => Any, Any) => Any
object INTEGER{
  def increment = (x: Any) => x.asInstanceOf[Int] + 1
  val init = 0
}
def ZERO(f: Any => Any, x: Any) = x
def ONE(f: Any => Any, x: Any) = f(x)
def TWO(f: Any => Any, x: Any) = f(f(x))
def THREE(f: Any => Any, x: Any) = f(f(f(x)))
def ADD(a: NUMBER, b: NUMBER)(increment: Any => Any, init: Any): Any = {
  a(increment, b(increment, init))
}
def MUL(a: NUMBER, b: NUMBER)(increment: Any => Any, init: Any): Any = {
  a(b(increment, _), init)
}

ADD(ONE, TWO)(INTEGER.increment, INTEGER.init)
MUL(TWO, THREE)(INTEGER.increment, 0)

ADD(ONE, MUL(TWO, THREE))(INTEGER.increment, INTEGER.init)


