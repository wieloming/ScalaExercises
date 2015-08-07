//TODO: dokoñczyæ Huffman algorithm

//P46 (**) Truth tables for logical expressions.
//Define functions and, or, nand, nor, xor, impl, and equ (for logical equivalence) which return true or false according to the result of their respective operations; e.g. and(A, B) is true if and only if both A and B are true.
def and(a: Boolean, b: Boolean) = a && b
and(a = true, b = true)
def xor(a: Boolean, b: Boolean) = (a || b) && a != b
xor(a = true, b = true)
def or(a: Boolean, b: Boolean) = a || b
or(a = true, b = true)
//Now, write a function called table2 which prints the truth table of a given logical expression in two variables.
def table2(f: (Boolean, Boolean) => Boolean) = {
  println("A    B    result")
  for (a <- List(true, false); b <- List(true, false)) {
    println(a + " " + b + " " + f(a, b))
  }
}
table2((a: Boolean, b: Boolean) => and(a, or(a, b)))
//P47 (*) Truth tables for logical expressions (2).
//  Continue problem P46 by redefining and, or, etc as operators. (i.e. make them methods of a new class with an implicit conversion from Boolean.) not will have to be left as a object method.
implicit class Logic(a: Boolean) {
  def and(b: Boolean) = a && b
  def xor(b: Boolean) = (a || b) && a != b
  def or(b: Boolean) = a || b
}
def not(b: Boolean) = !b
table2((a: Boolean, b: Boolean) => a and (a or not(b)))
//P48 (**) Truth tables for logical expressions (3).
//  Omitted for now.
//
//  P49 (**) Gray code.
//  An n-bit Gray code is a sequence of n-bit strings constructed according to certain rules. For example,
def gray(n: Int): List[String] = {
  if (n == 0) List("")
  else {
    val shorterCombinations = gray(n - 1)
    shorterCombinations.map("0" + _) ::: shorterCombinations.map("1" + _)
  }
}
gray(3)
//P50 (***) Huffman code.
//  First of all, consult a good book on discrete mathematics or algorithms for a detailed description of Huffman codes!
//
//  We suppose a set of symbols with their frequencies, given as a list of (S, F) Tuples. E.g. (("a", 45), ("b", 13), ("c", 12), ("d", 16), ("e", 9), ("f", 5)). Our objective is to construct a list of (S, C) Tuples, where C is the Huffman code word for the symbol S.
//
//  scala> huffman(List(("a", 45), ("b", 13), ("c", 12), ("d", 16), ("e", 9), ("f", 5)))
//res0: List[String, String] = List((a,0), (b,101), (c,100), (d,111), (e,1101), (f,1100))