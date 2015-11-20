package FunctionalConstructs

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

object FutureSequence extends App {

  def futureSequence[A](sf: Seq[Future[A]]): Future[Seq[A]] = sf match {
    case Nil => Future(Nil)
    case (head:Future[A]) :: tail =>
      futureSequence(tail)
        .flatMap(seqTail => head.map(Seq(_) ++ seqTail))
  }

  futureSequence(Seq(Future(1), Future(2), Future(3))).map(println)

  readLine()
}
