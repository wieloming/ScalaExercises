sealed abstract class Tree[+T]
case class Node[+T](value: T, left: Tree[T], right: Tree[T]) extends Tree[T] {
  override def toString = "T(" + value.toString + " " + left.toString + " " + right.toString + ")"
}
case object End extends Tree[Nothing] {
  override def toString = "."
}
object Node {
  def apply[T](value: T): Node[T] = Node(value, End, End)
}

//The example tree on the right is given by

Node('a',
  Node('b', Node('d'), Node('e')),
  Node('c', End, Node('f', Node('g'), End)))


//  P54 Omitted; our tree representation will only allow well-formed trees.
//Score one for static typing.
//
//P55 (**) Construct completely balanced binary trees.
//In a completely balanced binary tree, the following property holds for every node: The number of nodes in its left subtree and the number of nodes in its right subtree are almost equal, which means their difference is not greater than one.
//
//  Define an object named Tree. Write a function Tree.cBalanced to construct completely balanced binary trees for a given number of nodes. The function should generate all solutions. The function should take as parameters the number of nodes and a single value to put in all of them.
//
//scala> Tree.cBalanced(4, "x")
//res0: List(Node[String]) = List(T(x T(x . .) T(x . T(x . .))), T(x T(x . .) T(x T(x . .) .)), ...
//
//P56 (**) Symmetric binary trees.
//Let us call a binary tree symmetric if you can draw a vertical line through the root node and then the right subtree is the mirror image of the left subtree. Add an isSymmetric method to the Tree class to check whether a given binary tree is symmetric. Hint: Write an isMirrorOf method first to check whether one tree is the mirror image of another. We are only interested in the structure, not in the contents of the nodes.
//
//  scala> Node('a', Node('b'), Node('c')).isSymmetric
//res0: Boolean = true
//
//P57 (**) Binary search trees (dictionaries).
//  Write a function to add an element to a binary search tree.
//
//scala> End.addValue(2)
//res0: Node[Int] = T(2 . .)
//
//scala> res0.addValue(3)
//res1: Node[Int] = T(2 . T(3 . .))
//
//scala> res1.addValue(0)
//res2: Node[Int] = T(2 T(0 . .) T(3 . .))
//
//Hint: The abstract definition of addValue in Tree should be def addValue[U >: T <% Ordered[U]](x: U): Tree[U]. The >: T is because addValue's parameters need to be contravariant in T. (Conceptually, we're adding nodes above existing nodes. In order for the subnodes to be of type T or any subtype, the upper nodes must be of type T or any supertype.) The <% Ordered[U] allows us to use the < operator on the values in the tree.
//
//Use that function to construct a binary tree from a list of integers.
//
//  scala> Tree.fromList(List(3, 2, 5, 7, 1))
//res3: Node[Int] = T(3 T(2 T(1 . .) .) T(5 . T(7 . .)))
//
//Finally, use that function to test your solution to P56.
//
//  scala> Tree.fromList(List(5, 3, 18, 1, 4, 12, 21)).isSymmetric
//res4: Boolean = true
//
//scala> Tree.fromList(List(3, 2, 5, 7, 4)).isSymmetric
//res5: Boolean = false
//
//P58 (**) Generate-and-test paradigm.
//Apply the generate-and-test paradigm to construct all symmetric, completely balanced binary trees with a given number of nodes.
//
//  scala> Tree.symmetricBalancedTrees(5, "x")
//res0: List[Node[String]] = List(T(x T(x . T(x . .)) T(x T(x . .) .)), T(x T(x T(x . .) .) T(x . T(x . .))))
//
//P59 (**) Construct height-balanced binary trees.
//  In a height-balanced binary tree, the following property holds for every node: The height of its left subtree and the height of its right subtree are almost equal, which means their difference is not greater than one.
//
//  Write a method Tree.hbalTrees to construct height-balanced binary trees for a given height with a supplied value for the nodes. The function should generate all solutions.
//
//scala> Tree.hbalTrees(3, "x")
//res0: List[Node[String]] = List(T(x T(x T(x . .) T(x . .)) T(x T(x . .) T(x . .))), T(x T(x T(x . .) T(x . .)) T(x T(x . .) .)), ...
//
//P60 (**) Construct height-balanced binary trees with a given number of nodes.
//  Consider a height-balanced binary tree of height H. What is the maximum number of nodes it can contain? Clearly, MaxN = 2H - 1. However, what is the minimum number MinN? This question is more difficult. Try to find a recursive statement and turn it into a function minHbalNodes that takes a height and returns MinN.
//
//scala> minHbalNodes(3)
//res0: Int = 4
//
//On the other hand, we might ask: what is the maximum height H a height-balanced binary tree with N nodes can have? Write a maxHbalHeight function.
//
//  scala> maxHbalHeight(4)
//res1: Int = 3
//
//Now, we can attack the main problem: construct all the height-balanced binary trees with a given nuber of nodes.
//
//  scala> Tree.hbalTreesWithNodes(4, "x")
//res2: List[Node[String]] = List(T(x T(x T(x . .) .) T(x . .)), T(x T(x . T(x . .)) T(x . .)), ...
//
//Find out how many height-balanced trees exist for N = 15.