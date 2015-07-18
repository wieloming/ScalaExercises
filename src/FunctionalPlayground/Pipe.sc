def pipe(values: Function[_, _]*): Seq[Function[_, _]] = {
  if (values.size == 1) {
    Seq(values(0))
  }
  else if (values.size == 2) {
    val f1: Function[_, _] = values(0)
    val f2: Function[_, _] = values(1)
    (value: _) => f2(f1(value))
  }
  else {
    val newValues = pipe(values(0), values(1)) ++ values.tail.tail
    pipe(newValues)
  }
}

def returnOne: Function[Int, Int] = (num: Int) => 1
def addTwo(): Function[Int, Int] = (num) => num + 2

pipe(returnOne, addTwo())