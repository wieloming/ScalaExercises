def pipe[T](values: (T => T)*): T => T = values.toList match {
  case first :: Nil => values.head
  case first :: second :: Nil => (v: T) => second(first(v))
  case first :: second :: tail => pipe(Seq(pipe(first, second)) ++ tail: _*)
}

def addOne(): Int => Int = (num: Int) => num + 1
def addTwo(): Int => Int = (num) => num + 2
def addThree(): Int => Int = (num) => num + 3
pipe(addOne(), addTwo(), addThree())(8)

def addA(): String => String = (s: String) => s + "A"
def addB(): String => String = (s: String) => s + "B"
def addC(): String => String = (s: String) => s + "C"
pipe(addA(), addB(), addC())("")
