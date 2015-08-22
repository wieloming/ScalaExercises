//
//P01 (*) Find the last element of a list.
List(1, 1, 2, 3, 5, 8).last

//P02 (*) Find the last but one element of a list.
val list = List(1, 1, 2, 3, 5, 8)
list.init.last

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
