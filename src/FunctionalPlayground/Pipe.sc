def pipe[T](values: Function[T, T]*): Function[T, T] = {
  def pipeToSeq(values: Function[T, T]*): Seq[Function[T, T]] = {
    if (values.size == 1) {
      Seq(values(0))
    }
    else if (values.size == 2) {
      val f1: Function[T, T] = values(0)
      val f2: Function[T, T] = values(1)
      Seq((v:T) => f2(f1(v)))
    }
    else {
      val newValues: Seq[Function[T, T]] = pipeToSeq(values(0), values(1)) ++ values.tail.tail
      pipeToSeq(newValues:_*)
    }
  }
  pipeToSeq(values:_*).head
}
def addOne(): Function[Int, Int] = (num: Int) => num + 1
def addTwo(): Function[Int, Int] = (num) => num + 2
def addThree(): Function[Int, Int] = (num) => num + 3
pipe(addOne(), addTwo(), addThree())(8)