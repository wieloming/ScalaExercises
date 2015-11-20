import scalaz.Scalaz._

(1 |-> 50) filter { x => x.shows contains '7' }