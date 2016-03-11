//Church numerals
type NUMBER[T] = (T => T, T) => T

def ZERO[T](f: T => T, x: T) = x
def ONE[T](f: T => T, x: T) = f(x)
def TWO[T](f: T => T, x: T) = f(f(x))
def THREE[T](f: T => T, x: T) = f(f(f(x)))

def ADD[T](a: NUMBER[T], b: NUMBER[T])(increment: T => T, init: T): T = {
  a(increment, b(increment, init))
}
def MUL[T](a: NUMBER[T], b: NUMBER[T])(increment: T => T, init: T): T = {
  a(b(increment, _), init)
}

ADD[Int](ONE, TWO)((x) => x + 1, 0)
MUL[Int](TWO, THREE)((x) => x + 1, 0)

ADD[Int](ONE, MUL(TWO, THREE))((x) => x + 1, 0)
ADD[String](ONE, MUL(TWO, THREE))((x) => x + "o", "")

