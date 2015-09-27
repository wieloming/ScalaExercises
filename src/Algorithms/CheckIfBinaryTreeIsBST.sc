import stuff.Tree
//    4
//  2  6
// 1 3 5 7
val tree = new Tree(4)
tree.left = new Tree(2)
tree.right = new Tree(6)
tree.left.left = new Tree(1)
tree.left.right = new Tree(5)
tree.right.left = new Tree(5)
tree.right.right = new Tree(7)

def checkIfTreeIsBST(tree: Tree, small: Int, large: Int): Boolean = {
  if (tree == null) {
    true
  }else if(tree.value <= large && tree.value >= small){
    checkIfTreeIsBST(tree.left, small, tree.value) && checkIfTreeIsBST(tree.right, tree.value, large)
  }else{
    false
  }
}
checkIfTreeIsBST(tree, Int.MinValue, Int.MaxValue)