//
//P01 (*) Find the last element of a list.
List(1, 1, 2, 3, 5, 8).last

//P02 (*) Find the last but one element of a list.
val list = List(1, 1, 2, 3, 5, 8)
list(list.size - 2)

//P03 (*) Find the Kth element of a list.
val k = 2
list(k)
//P04 (*) Find the number of elements of a list.
list.size
//P05 (*) Reverse a list.
list.reverse
//P06 (*) Find out whether a list is a palindrome.
def isPalindrome[T](list: List[T]) = {
  list.nonEmpty && list == list.reverse
}
isPalindrome(list)
//P07 (**) Flatten a nested list structure.
def flatten(list: List[Any]): List[Int] = list match {
  case Nil => Nil
  case (head: Int) :: tail => head :: flatten(tail)
  case (head: List[Int]) :: tail => flatten(head) ++ flatten(tail)
}
val listToFlatten = List(List(1, 1), 2, List(3, List(5, 8)))
flatten(listToFlatten)
//P08 (**) Eliminate consecutive duplicates of list elements.
val list2 = List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)
list2.distinct
//P09 (**) Pack consecutive duplicates of list elements into sublists.
val list3 = List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)
val list4 = list3.groupBy(_.toString()).map({ case (key, value) => value })
//P10 (*) Run-length encoding of a list.
//  Use the result of problem P09 to implement the so-called run-length encoding data compression method. Consecutive duplicates of elements are encoded as tuples (N, E) where N is the number of duplicates of the element E.
val list5 = list4.map(list => list.size -> list.head).toMap
//P11 (*) Modified run-length encoding.
//Modify the result of problem P10 in such a way that if an element has no duplicates it is simply copied into the result list. Only elements with duplicates are transferred as (N, E) terms.
list5.map({ case (key, value) =>
  if (key == 1) value
  else (key, value)
})
//P12 (**) Decode a run-length encoded list.
list5.flatMap({case(key, value) => List.fill(key)(value)}).toList.sortBy(_.toString())
//P13 (**) Run-length encoding of a list (direct solution).
//  Implement the so-called run-length encoding data compression method directly. I.e. don't use other methods you've written (like P09's pack); do all the work directly.
val list6 = List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)
list6.groupBy(_.toString()).map({case(key, value) => List(value.size, key.toString)})
//P14 (*) Duplicate the elements of a list.
val list7 = List('a, 'b, 'c, 'c, 'd)
list7.flatMap(el => List(el, el))
//P15 (**) Duplicate the elements of a list a given number of times.
def duplicateN(n: Int, list: List[Any]) = list.flatMap(el => List.fill(n)(el))
duplicateN(3, List('a, 'b, 'c, 'c, 'd))
//P16 (**) Drop every Nth element from a list.
//Example:
//
//  scala> drop(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
//res0: List[Symbol] = List('a, 'b, 'd, 'e, 'g, 'h, 'j, 'k)
//
//P17 (*) Split a list into two parts.
//  The length of the first part is given. Use a Tuple for your result.
//
//Example:
//
//  scala> split(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
//res0: (List[Symbol], List[Symbol]) = (List('a, 'b, 'c),List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
//
//P18 (**) Extract a slice from a list.
//  Given two indices, I and K, the slice is the list containing the elements from and including the Ith element up to but not including the Kth element of the original list. Start counting the elements with 0.
//
//Example:
//
//  scala> slice(3, 7, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
//res0: List[Symbol] = List('d, 'e, 'f, 'g)
//
//P19 (**) Rotate a list N places to the left.
//  Examples:
//
//  scala> rotate(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
//res0: List[Symbol] = List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k, 'a, 'b, 'c)
//
//scala> rotate(-2, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
//res1: List[Symbol] = List('j, 'k, 'a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i)
//
//P20 (*) Remove the Kth element from a list.
//Return the list and the removed element in a Tuple. Elements are numbered from 0.
//
//Example:
//
//  scala> removeAt(1, List('a, 'b, 'c, 'd))
//res0: (List[Symbol], Symbol) = (List('a, 'c, 'd),'b)
//
//P21 (*) Insert an element at a given position into a list.
//  Example:
//
//  scala> insertAt('new, 1, List('a, 'b, 'c, 'd))
//res0: List[Symbol] = List('a, 'new, 'b, 'c, 'd)
//
//P22 (*) Create a list containing all integers within a given range.
//  Example:
//
//  scala> range(4, 9)
//res0: List[Int] = List(4, 5, 6, 7, 8, 9)
//
//P23 (**) Extract a given number of randomly selected elements from a list.
//Example:
//
//  scala> randomSelect(3, List('a, 'b, 'c, 'd, 'f, 'g, 'h))
//res0: List[Symbol] = List('e, 'd, 'a)
//
//Hint: Use the solution to problem P20
//  P24 (*) Lotto: Draw N different random numbers from the set 1..M.
//  Example:
//
//  scala> lotto(6, 49)
//res0: List[Int] = List(23, 1, 17, 33, 21, 37)
//
//P25 (*) Generate a random permutation of the elements of a list.
//  Hint: Use the solution of problem P23.
//
//Example:
//
//  scala> randomPermute(List('a, 'b, 'c, 'd, 'e, 'f))
//res0: List[Symbol] = List('b, 'a, 'd, 'c, 'e, 'f)
//
//P26 (**) Generate the combinations of K distinct objects chosen from the N elements of a list.
//In how many ways can a committee of 3 be chosen from a group of 12 people? We all know that there are C(12,3) = 220 possibilities (C(N,K) denotes the well-known binomial coefficient). For pure mathematicians, this result may be great. But we want to really generate all the possibilities.
//
//  Example:
//
//  scala> combinations(3, List('a, 'b, 'c, 'd, 'e, 'f))
//res0: List[List[Symbol]] = List(List('a, 'b, 'c), List('a, 'b, 'd), List('a, 'b, 'e), ...
//
//P27 (**) Group the elements of a set into disjoint subsets.
//a) In how many ways can a group of 9 people work in 3 disjoint subgroups of 2, 3 and 4 persons? Write a function that generates all the possibilities.
//
//  Example:
//
//  scala> group3(List("Aldo", "Beat", "Carla", "David", "Evi", "Flip", "Gary", "Hugo", "Ida"))
//res0: List[List[List[String]]] = List(List(List(Aldo, Beat), List(Carla, David, Evi), List(Flip, Gary, Hugo, Ida)), ...
//
//b) Generalize the above predicate in a way that we can specify a list of group sizes and the predicate will return a list of groups.
//
//Example:
//
//  scala> group(List(2, 2, 5), List("Aldo", "Beat", "Carla", "David", "Evi", "Flip", "Gary", "Hugo", "Ida"))
//res0: List[List[List[String]]] = List(List(List(Aldo, Beat), List(Carla, David), List(Evi, Flip, Gary, Hugo, Ida)), ...
//
//Note that we do not want permutations of the group members; i.e. ((Aldo, Beat), ...) is the same solution as ((Beat, Aldo), ...). However, we make a difference between ((Aldo, Beat), (Carla, David), ...) and ((Carla, David), (Aldo, Beat), ...).
//
//You may find more about this combinatorial problem in a good book on discrete mathematics under the term "multinomial coefficients".
//  P28 (**) Sorting a list of lists according to length of sublists.
//  a) We suppose that a list contains elements that are lists themselves. The objective is to sort the elements of the list according to their length. E.g. short lists first, longer lists later, or vice versa.
//
//  Example:
//
//  scala> lsort(List(List('a, 'b, 'c), List('d, 'e), List('f, 'g, 'h), List('d, 'e), List('i, 'j, 'k, 'l), List('m, 'n), List('o)))
//res0: List[List[Symbol]] = List(List('o), List('d, 'e), List('d, 'e), List('m, 'n), List('a, 'b, 'c), List('f, 'g, 'h), List('i, 'j, 'k, 'l))
//
//b) Again, we suppose that a list contains elements that are lists themselves. But this time the objective is to sort the elements according to their length frequency; i.e. in the default, sorting is done ascendingly, lists with rare lengths are placed, others with a more frequent length come later.
//
//Example:
//
//  scala> lsortFreq(List(List('a, 'b, 'c), List('d, 'e), List('f, 'g, 'h), List('d, 'e), List('i, 'j, 'k, 'l), List('m, 'n), List('o)))
//res1: List[List[Symbol]] = List(List('i, 'j, 'k, 'l), List('o), List('a, 'b, 'c), List('f, 'g, 'h), List('d, 'e), List('d, 'e), List('m, 'n))
//
//Note that in the above example, the first two lists in the result have length 4 and 1 and both lengths appear just once. The third and fourth lists have length 3 and there are two list of this length. Finally, the last three lists have length 2. This is the most frequent length.
