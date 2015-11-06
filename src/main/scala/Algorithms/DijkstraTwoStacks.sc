var valueStack = collection.mutable.MutableList[Int]()
var operatorStack = collection.mutable.MutableList[(Int, Int) => Int]()
def popValue = {
  val value = valueStack.head
  valueStack = valueStack.tail
  value
}
def popOperator = {
  val value = operatorStack.head
  operatorStack = operatorStack.tail
  value
}
def pushValue(value: Int) = {
  valueStack = collection.mutable.MutableList(value) ++ valueStack
}
def pushOperator(value: (Int, Int) => Int) = {
  operatorStack = collection.mutable.MutableList(value) ++ operatorStack
}


def count(string: String): Unit = string.foreach {
  case '+' => pushOperator(_ + _)
  case '-' => pushOperator(_ - _)
  case '*' => pushOperator(_ * _)
  case '/' => pushOperator(_ / _)
  case '(' =>
  case ' ' =>
  case ')' =>
    val first = popValue
    val second = popValue
    val operator = popOperator
    pushValue(operator(first, second))
  case value => pushValue(value.asDigit)
}
count("( 1 + ( ( 2 + 3 ) * ( 4 * 5 ) ) )")
println(valueStack)
println(operatorStack)