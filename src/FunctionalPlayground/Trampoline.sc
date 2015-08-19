sealed trait Bounce[A]
case class Done[A](result: A) extends Bounce[A]
case class Call[A](thunk: () => Bounce[A]) extends Bounce[A]

def trampoline[A](bounce: Bounce[A]): A = bounce match {
  case Call(thunk) => trampoline(thunk())
  case Done(x) => x
}

def even2(n: Int): Bounce[Boolean] = {
  if (n == 0) Done(true)
  else Call(() => odd2(n - 1))
}
def odd2(n: Int): Bounce[Boolean] = {
  if (n == 0) Done(false)
  else Call(() => even2(n - 1))
}
trampoline(even2(9999))