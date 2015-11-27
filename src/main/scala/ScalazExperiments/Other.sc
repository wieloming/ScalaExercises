import scalaz._
import Scalaz._

"pac".println

//write function applied to the end of expression
1 + 2 + 3 |> (_.point[List])



