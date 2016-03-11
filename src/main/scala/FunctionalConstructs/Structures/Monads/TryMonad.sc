abstract class TRY

case class SUCCESS(value: Any) extends TRY

case class FAIL(value: Any) extends TRY

def TRY[T, F](fn: T => F) = (value: T) => {
  try {
    SUCCESS(fn(value))
  } catch {
    case e: Exception => FAIL(e.getMessage)
  }
}

def pipeTRY[T](values: Function[T, TRY]*): Function[T, TRY] = {
  def pipeToSeq(values: Function[T, TRY]*): Seq[Function[T, TRY]] = {
    values.toList match {
      case f1 :: Nil => Seq(values(0))
      case f1 :: f2 :: Nil => Seq((v: T) => {
        f1(v) match {
          case SUCCESS(x: T) => f2(x)
          case FAIL(x) => FAIL(x)
        }
      })
      case f1 :: f2 :: tail =>
        val newValues = pipeToSeq(f1, f2) ++ tail
        pipeToSeq(newValues: _*)
    }
  }
  pipeToSeq(values: _*).head
}

def enterOneOrGetException(value: Int): Int = value match {
  case 1 => 1
  case _ => throw new Exception("dupa")
}
def addOne(x: Int) = x + 1

TRY(addOne)(2)
TRY(enterOneOrGetException)(1)
TRY(enterOneOrGetException)(2)

println(pipeTRY(TRY(enterOneOrGetException), TRY(addOne))(1))