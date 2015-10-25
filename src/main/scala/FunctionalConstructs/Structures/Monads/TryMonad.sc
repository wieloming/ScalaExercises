abstract class TRY
case class SUCCESS(value: Any) extends TRY
case class FAIL(value: Any) extends TRY

def TRY[T, F](fn: T=>F) = (value: T) => {
  try{
    SUCCESS(fn(value))
  }catch{
    case e:Exception => FAIL(e.getMessage)
  }
}

def pipeTRY[T](values: Function[T, TRY]*): Function[T, TRY] = {
  def pipeToSeq(values: Function[T, TRY]*): Seq[Function[T, TRY]] = {
    if (values.size == 1) {
      Seq(values(0))
    }
    else if (values.size == 2) {
      val f1: Function[T, TRY] = values(0)
      val f2: Function[T, TRY] = values(1)
      Seq((v:T) => {
        val retVal = f1(v)
        retVal match {
          case SUCCESS(x:T) => f2(x)
          case FAIL(x) => FAIL(x)
        }
      })
    } else {
      val newValues: Seq[Function[T, TRY]] = pipeToSeq(values(0), values(1)) ++ values.tail.tail
      pipeToSeq(newValues:_*)
    }
  }
  pipeToSeq(values:_*).head
}

def enterOneOrGetException(value: Int):Int = value match {
  case 1 => 1
  case _ => throw new Exception("dupa")
}
def addOne(x: Int) = x + 1

TRY(addOne)(2)
TRY(enterOneOrGetException)(1)
TRY(enterOneOrGetException)(2)

println(pipeTRY(TRY(enterOneOrGetException),TRY(addOne))(1))