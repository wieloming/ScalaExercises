import scalaz._
import Scalaz._

"event 1 ok".success[String]
"event 1 failed!".failure[String]

("ev 1 ok".success[String] |@| "ev 2 failed!".failure[String]){_ + _}