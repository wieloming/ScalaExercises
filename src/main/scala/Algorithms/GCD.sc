def GCD(smaller: Int, bigger: Int):Int = {
  require(bigger > smaller)
  if(bigger % smaller == 0) smaller
  else GCD(bigger % smaller, smaller)
}
GCD(1,5)
GCD(10,100)
GCD(22,131)