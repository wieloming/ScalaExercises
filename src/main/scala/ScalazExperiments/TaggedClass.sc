import scalaz._
trait Money
def printMoney(amount: Int @@ Money) = println(amount)
val dolars = Tag[Int, Money](12)

printMoney(dolars)
//printMoney(12)