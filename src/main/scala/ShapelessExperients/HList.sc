import shapeless._

val hlist1 = 1 :: "foo" :: true :: HNil
val hlist2 = "foo" :: 2 :: true :: HNil

def thirdIsTrue(hlist: HList) = hlist match {
  case (_:Int) :: (_:String) :: true :: HNil => true
  case _ => false
}

thirdIsTrue(hlist1)
thirdIsTrue(hlist2)
