type function[T, M] = T => M

def pipe(values: _*): Any ={
  if(values.size == 1){
    Seq(values(0))
  }
  else if(values.size == 2){
    val f1 = values(0)
    val f2 = values(1)
    (value:Any) => f2(f1(value))
  }
  else {
    val newValues = pipe(values(0), values(1)) ++ values.tail.tail
    pipe(newValues)
  }
}

def returnOne:function[Int, Int] = (num) => 1
def addTwo():function[Int, Int] = (num) => num + 2

pipe(returnOne, addTwo)