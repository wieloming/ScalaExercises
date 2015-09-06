implicit class uberInt(n: Int) {
  def ! :BigInt = (BigInt(1) to n).product
}

def count(n: Int): BigInt = ((n*2)!) / ((n !) * (n !))

println("result: ")
count(20)