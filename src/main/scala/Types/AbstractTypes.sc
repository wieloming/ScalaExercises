trait SimplestContainer {
  type A
  // Abstract Type Member
  def value: A
}

object IntContainer extends SimplestContainer {
  type A = Int
  def value = 42
}
trait OnlyNumbersContainer {
  type A <: Number
  def value: A
}

