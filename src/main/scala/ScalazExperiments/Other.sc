import scalaz._
import Scalaz._

"pac".println

//write function applied to the end of expression
1 + 2 + 3 |> (_.point[List])

//unfold
val unfoldList = unfold(10) { (x: Int) => if (x == 0) None else Some(x, x - 1) }
unfoldList.toList

//unite
List(Some(1), None).unite

//memoization
def expensive(n: Int) = n * 2
val memo = Memo.immutableHashMapMemo {
  n: Int => expensive(n)
}

memo(1000)
