package FunctionalConstructs

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

object ExtendedFuture extends App {

  def futureSequence[A](sf: Seq[Future[A]]): Future[Seq[A]] = sf match {
    case Nil => Future(Nil)
    case (head:Future[A]) :: tail =>
      futureSequence(tail)
        .flatMap(seqTail => head.map(Seq(_) ++ seqTail))
  }
  def sequentialTraverse[T, U](items: Seq[T], chunkSize: Int)(function: T => Future[U]): Future[Seq[U]] = {
    items.grouped(chunkSize).foldLeft(Future.successful[Seq[Seq[U]]](Nil)) {
      (f, group) => f.flatMap(x => Future.traverse(group)(function).map(_+:x))
    }.map(_.flatten)
  }
  def optTraverse[T, U](option: Option[T])(function: T => Future[U]): Future[Option[U]] = {
    option match {
      case Some(el) => function(el).map(Some(_))
      case None => Future.successful(None)
    }
  }
  def flatTraverse[A, B, F[B] <: Iterable[B]](in: Seq[A])(fn: A => Future[F[B]]): Future[Seq[B]] =
    in.foldLeft(Future.successful[Seq[B]](Nil)) { (acc, el) =>
      for (r <- acc; b <- fn(el)) yield r ++ b
    }
  futureSequence(Seq(Future(1), Future(2), Future(3))).map(println)
  flatTraverse(Seq(1,2,3))(x => Future(Seq(x)))
  readLine()
}
