def countChange(money: Int, coins: List[Int]): Int = {
   if (money == 0) 1
    else if (money < 0) 0
    else if (coins.isEmpty && money > 0) 0
    else countChange(money, coins.tail) + countChange(money - coins.head, coins)
}
countChange(4, List(1, 2))
countChange(4, List(2, 1))