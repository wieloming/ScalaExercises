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
MakeBigger(10 :: "dupa" :: HNil)

//POLYMAP (magic, damn magic)!
object PolyMap extends Poly2 {
  implicit def hnilCase[F <: Poly1]: Case.Aux[F, HNil, HNil] =
    at[F, HNil]((f, l) => HNil)

  implicit def hconsCase[F <: Poly1, H, T <: HList, FT <: HList]
  (implicit headCase: poly.Case[F, H :: HNil],tailMap: Case.Aux[F, T, FT]) : Case.Aux[F, H :: T, headCase.Result :: FT] =
    at[F, H :: T]((f: F, l: H :: T) => headCase(l.head) :: tailMap(f, l.tail))
}

PolyMap(MakeBigger, 10 :: "dupa" :: HNil)