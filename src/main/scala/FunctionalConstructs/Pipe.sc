def pipe[T](values: (T => T)*): T => T = {
  def pipeToSeq(values: (T => T)*): Seq[T => T] = {
    if (values.size == 1) {
      Seq(values.head)
    } else if (values.size == 2) {
      val f1: T => T = values.head
      val f2: T => T = values.tail.head
      Seq((v: T) => f2(f1(v)))
    } else {
      val newValues: Seq[T => T] = pipeToSeq(values.head, values.tail.head) ++ values.tail.tail
      pipeToSeq(newValues: _*)
    }
  }
  pipeToSeq(values: _*).head
}
def addOne(): Int => Int = (num: Int) => num + 1
def addTwo(): Int => Int = (num) => num + 2
def addThree(): Int => Int = (num) => num + 3
pipe(addOne(), addTwo(), addThree())(8)

def addA(): String => String = (s: String) => s + "A"
def addB(): String => String = (s: String) => s + "B"
def addC(): String => String = (s: String) => s + "C"
pipe(addA(), addB(), addC())("")
