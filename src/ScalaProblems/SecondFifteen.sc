import scala.util.Random

//P16 (**) Drop every Nth element from a list.
def drop[T](num: Int, list: List[T]) = {
  list
    .zipWithIndex
    .foldLeft(List[T]()) { (acc, tuple) => tuple match {
    case (el, pos) => if (pos + 1 % num == 0) acc else el :: acc
  }
  }
    .sortBy(_.toString)
}
drop(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
//P17 (*) Split a list into two parts.
def split[T](num: Int, list: List[T]) = {
  list.splitAt(3)
}
split(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
//P18 (**) Extract a slice from a list.
def slice[T](start: Int, end: Int, list: List[T]) = {
  list.slice(start, end)
}
slice(3, 7, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
//P19 (**) Rotate a list N places to the left.
def rotate[T](nums: Int, list: List[T]): List[T] = {
  if (nums > 0) return rotate(nums - 1, list.tail :+ list.head)
  if (nums < 0) return rotate(nums + 1, list.last :: list.init)
  list
}
rotate(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
rotate(-2, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
//P20 (*) Remove the Kth element from a list.
def removeAt[T](num: Int, list: List[T]) = {
  val element = list(num)
  List(list.filter(_ != element), element)
}
removeAt(1, List('a, 'b, 'c, 'd))
//P21 (*) Insert an element at a given position into a list.
def insertAt[T](element: T, position: Int, list: List[T]) = {
  val lists = list.splitAt(position)
  lists._1 ++ List(element) ++ lists._2
}
insertAt('new, 1, List('a, 'b, 'c, 'd))
//P22 (*) Create a list containing all integers within a given range.
def range(start: Int, end: Int) = {
  (start to end).toList
}
range(4, 9)
//P23 (**) Extract a given number of randomly selected elements from a list.
def randomSelect[T](num: Int, list: List[T]) = {
  Random.shuffle(list).take(num)
}
randomSelect(3, List('a, 'b, 'c, 'd, 'f, 'g, 'h))
//P24 (*) Lotto: Draw N different random numbers from the set 1..M.
def lotto(howMany: Int, largest: Int) = {
  Random.shuffle((1 to largest).toList).take(howMany)
}
lotto(6, 49)
//P25 (*) Generate a random permutation of the elements of a list.
def randomPermute[T](list: List[T]): List[T] = {
  Random.shuffle(list)
}
randomPermute(List('a, 'b, 'c, 'd, 'e, 'f))
//P26 (**) Generate the combinations of K distinct objects chosen from the N elements of a list.
//In how many ways can a committee of 3 be chosen from a group of 12 people? We all know that there are C(12,3) = 220 possibilities (C(N,K) denotes the well-known binomial coefficient). For pure mathematicians, this result may be great. But we want to really generate all the possibilities.
def combinations[T](howMany: Int, list: List[T]): List[List[T]] = {
  if (howMany == 0) List(Nil)
  else if (howMany > list.size) Nil
  else if (howMany == list.size) List(list)
  else combinations(howMany - 1, list.tail).map(
    list.head :: _
  ) ::: combinations(howMany, list.tail)
}
combinations(3, List('a, 'b, 'c, 'd, 'e, 'f))
//P27 (**) Group the elements of a set into disjoint subsets.
//a) In how many ways can a group of 9 people work in 3 disjoint subgroups of 2, 3 and 4 persons? Write a function that generates all the possibilities.
def group3[T](list: List[T]): List[List[List[T]]] = {
  val number = list.size
  (for {
    a <- 2 to number
    b <- 2 to number if number != a
    c <- 2 to number if number != b
  } yield List(a, b, c)).filter(_.sum == number).toList.map({
    case List(a, b, c) =>
      List(list.slice(0, a), list.slice(a, a + b), list.slice(a + b, b + c + 1))
  }).distinct
}

group3(List("Aldo", "Beat", "Carla", "David", "Evil", "Flip", "Gary", "Hugo", "Ida"))
//You may find more about this combinatorial problem in a good book on discrete mathematics under the term "multinomial coefficients".
//  P28 (**) Sorting a list of lists according to length of sublists.
def lsort[T](list: List[List[T]]): List[List[T]] = {
  list.sortBy(_.size)
}
  lsort(List(List('a, 'b, 'c), List('d, 'e), List('f, 'g, 'h), List('d, 'e), List('i, 'j, 'k, 'l), List('m, 'n), List('o)))
//b) Again, we suppose that a list contains elements that are lists themselves. But this time the objective is to sort the elements according to their length frequency; i.e. in the default, sorting is done ascendingly, lists with rare lengths are placed, others with a more frequent length come later.
def lsortFreq[T](list: List[List[T]]) = {
  list.groupBy(_.size).toList.sortBy(_._2.size).map(_._2)
}
lsortFreq(List(List('a, 'b, 'c), List('d, 'e), List('f, 'g, 'h), List('d, 'e), List('i, 'j, 'k, 'l), List('m, 'n), List('o)))
