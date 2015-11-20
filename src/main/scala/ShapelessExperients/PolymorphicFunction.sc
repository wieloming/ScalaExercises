import shapeless._

object MakeBigger extends Poly1 {

  implicit def intCase = at[Int](_ * 100)

  implicit def stringCase = at[String](_.toUpperCase)

  // a bigger empty list is still empty
  implicit def hnilCase = at[HNil](identity)

  // copy all elements
  implicit def hconsCase[H, T <: HList]
  (implicit tailCase: MakeBigger.Case[T] {type Result <: HList}) =
    at[H :: T](l => l.head :: l.head :: MakeBigger(l.tail))
}

MakeBigger(10)
MakeBigger("dupa")