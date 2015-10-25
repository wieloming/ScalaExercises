def findBalancePoint(list:List[Int]): Option[Int] ={
  def findBalancePoint(list:List[Int], index: Int):Option[Int] = {
    if(index < list.length) {
      val sumRight = list.drop(index + 1).sum
      val sumLeft = list.take(index).sum
      if (sumLeft == sumRight) Some(sumLeft)
      else findBalancePoint(list, index + 1)
    }else{
      None
    }
  }
  findBalancePoint(list, 0)
}
findBalancePoint(List(1, 2, 9, 4, -1))
findBalancePoint(List(1, 2, 9, 4, -1, 3))