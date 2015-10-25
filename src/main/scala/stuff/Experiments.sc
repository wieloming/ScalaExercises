object s_+: {
  def unapply(s: String): Option[(Char, String)] = s.headOption.map {
    (_, s.tail)
  }
}

def areStringsEqual(x: String, y: String): Boolean = (x, y) match {
  case ("", "") => true
  case (xh s_+: xt, yh s_+: yt) => x.head == y.head && areStringsEqual(x.tail, y.tail)
  case (_, _) => false
}

println(areStringsEqual("pac", "pac"))
println(areStringsEqual("pac", "pac2"))

