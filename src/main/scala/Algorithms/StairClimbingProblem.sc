import scala.collection.mutable

def climb(n: Int): List[List[Int]] = {
  def climb(n: Int, current: List[Int]): List[List[Int]] = {
    if (n == 0) return List(current)
    if (n < 0) Nil
    else climb(n - 1, 1 :: current) ++ climb(n - 2, 2 :: current) ++ climb(n - 3, 3 :: current)
  }
  climb(n, List.empty)
}
def climbOptimized(n: Int): List[List[Int]] = {
  if (n == 1) return List(List(1))
  if (n == 2) return List(List(1, 1), List(2))
  if (n == 3) List(List(1, 1, 1), List(1, 2), List(2, 1), List(3))
  else climbOptimized(n - 1) ++ climbOptimized(n - 2) ++ climbOptimized(n - 3)
}
def climbEvenMoreOptimazed(n: Int): mutable.MutableList[List[Int]] = {
  val resultFor1 = mutable.MutableList(List(1))
  val resultFor2 = mutable.MutableList(List(1, 1), List(2))
  val resultFor3 = mutable.MutableList(List(1, 1, 1), List(1, 2), List(2, 1), List(3))
  if (n == 1) return resultFor1
  if (n == 2) return resultFor2
  if (n == 3) return resultFor3
  val previous = mutable.MutableList(resultFor1, resultFor2, resultFor3)
  var current = 3
  while(current < n){
    val preTotal = previous.head ++ previous(1) ++ previous(2)
    previous(0) = previous(1)
    previous(1) = previous(2)
    previous(2) = preTotal
    current += 1
  }
  previous(2)
}
val startTime = System.currentTimeMillis()
climb(21)
println("climb time: " + (System.currentTimeMillis() - startTime))
val startTime2 = System.currentTimeMillis()
climbOptimized(21)
println("climb time optimazed: " + (System.currentTimeMillis() - startTime2))
val startTime3 = System.currentTimeMillis()
climbEvenMoreOptimazed(21)
println("climb time even more optimazed: " + (System.currentTimeMillis() - startTime3))

