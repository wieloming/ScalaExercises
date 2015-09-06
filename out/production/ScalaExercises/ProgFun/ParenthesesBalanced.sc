def balance(chars: List[Char]): Boolean = {
  def balance(chars: List[Char], openedPars: Int):Boolean = {
    if (chars.isEmpty) openedPars == 0
    else if (chars.head == '(') balance(chars.tail, openedPars + 1)
    else if (chars.head == ')' && openedPars < 1) false
    else if (chars.head == ')') balance(chars.tail, openedPars - 1)
    else balance(chars.tail, openedPars)
  }
  balance(chars, 0)
}

balance("(if (zero? x) max (/ 1 x))".toList)
balance("I told him (that it’s not (yet) done). (But he wasn’t listening)".toList)
balance(":-)".toList)
balance("())(".toList)