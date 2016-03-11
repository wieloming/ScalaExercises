object s_+: {
  def unapply(s: String): Option[(Char, String)] = s.headOption.map {
    (_, s.tail)
  }
}

def areStringsEqual(a: String, b: String): Boolean = (a, b) match {
  case ("", "") => true
  case (x s_+: xs, y s_+: ys) => x == y && areStringsEqual(xs, ys)
  case (_, _) => false
}

println(areStringsEqual("pac", "pac"))
println(areStringsEqual("pac", "pac2"))

