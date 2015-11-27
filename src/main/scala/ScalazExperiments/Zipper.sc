import scalaz._
import Scalaz._


val zipper = for {
  z <- Stream(1, 2, 3, 4).toZipper
  n1 <- z.next
  n2 <- n1.next
} yield { n2.modify {_ => 7} }

zipper.get.toList