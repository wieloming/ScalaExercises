package Algorithms

import stuff.Tree

class FindClosestNumberInBinarySearchTree {
  def count(tree: Tree, number: Int): Any = {

    def checkDifferenceAndCountIfBigger(value: Int, branch: Tree): Unit = {
      val difference = Math.abs(value - number)
      val deeperDifference = Math.abs(branch.value - number)
      if (difference <= deeperDifference) {
        println(value)
      } else {
        count(branch, number)
      }
    }

    if (tree == null) return
    if (tree.value == number || (tree.left == null && tree.right == null)) println(tree.value)
    if (tree.left != null && tree.right == null) {
      checkDifferenceAndCountIfBigger(tree.value, tree.left)
    }
    if (tree.right != null && tree.left == null) {
      checkDifferenceAndCountIfBigger(tree.value, tree.right)
    }
    if (tree.left != null && tree.right != null) {
      val difference = Math.abs(tree.value - number)
      val deeperRightDifference = Math.abs(tree.right.value - number)
      val deeperLeftDifference = Math.abs(tree.left.value - number)
      var smallerDifference = deeperRightDifference
      var closer = tree.right
      if (deeperRightDifference > deeperLeftDifference) {
        smallerDifference = deeperLeftDifference
        closer = tree.left
      }
      if (difference <= smallerDifference) {
        System.out.println(tree.value)
      } else {
        count(closer, number)
      }
    }
  }
}
