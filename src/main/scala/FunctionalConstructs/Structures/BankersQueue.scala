package FunctionalConstructs.Structures

class BankersQueue[+A] private(val frontSize: Int,
                               val front: Stream[A],
                               val rearSize: Int,
                               val rear: Stream[A]) {

  val length = frontSize + rearSize

  def isEmpty = length == 0

  def head: A = front match {
    case head #:: _ => head
    case _ => throw new NoSuchElementException("queue empty")
  }

  def tail: BankersQueue[A] = front match {
    case _ #:: tail => moveAllFrontToRearIfNecessary(new BankersQueue(frontSize - 1, tail, rearSize, rear))
    case _ => throw new NoSuchElementException("queue empty")
  }

  def apply(number: Int): A = {
    if (number < frontSize) front(number)
    else if (number < frontSize + rearSize) rear(rearSize - (number - frontSize) - 1)
    else throw new NoSuchElementException("index out of range: " + number)
  }

  def enqueue[B >: A](number: B) = {
    def addToRear(number: B): BankersQueue[B] = new BankersQueue(frontSize, front, rearSize + 1, number #:: (rear: Stream[B]))
    moveAllFrontToRearIfNecessary(addToRear(number))
  }

  def dequeue: (A, BankersQueue[A]) = front match {
    case hd #:: tail => (hd, moveAllFrontToRearIfNecessary(new BankersQueue(frontSize - 1, tail, rearSize, rear)))
    case _ => throw new NoSuchElementException("dequeue on empty queue")
  }

  private def moveAllFrontToRearIfNecessary[B](queue: BankersQueue[B]) = {
    def moveAllRearToFront(queue: BankersQueue[B]): BankersQueue[B] = {
      new BankersQueue(queue.frontSize + queue.rearSize, queue.front ++ queue.rear.reverse, 0, Stream())
    }

    if (queue.rearSize <= queue.frontSize) queue
    else moveAllRearToFront(queue)
  }
}

object BankersQueue {
  def empty[A] = new BankersQueue[A](0, Stream(), 0, Stream())
  def apply[A](xs: A*) = new BankersQueue(xs.length, xs.reverse.toStream, 0, Stream())
}