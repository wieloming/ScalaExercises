package Algorithms

import stuff.Tree

class FindAllElementsOfLevelInBinaryTree {

  type LevelNum = Int

  def find(tree: Tree, levelToSearch: LevelNum):Unit = {
    findOnLevelRec(tree, 0, levelToSearch).foreach(println)
  }

  private def findOnLevelRec(tree: Tree, current: LevelNum, toFind: LevelNum): List[Int] = {
    if (tree == null || current > toFind) return List.empty
    if (current == toFind) return List(tree.value)
    findOnLevelRec(tree.left, current + 1, toFind) ++ findOnLevelRec(tree.right, current + 1, toFind)
  }
}
