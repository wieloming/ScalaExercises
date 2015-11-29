trait BOOL {
  type &&[B <: BOOL] <: BOOL          //def(b: Bool): Bool
  type ||[B <: BOOL] <: BOOL          //def(b: Bool): Bool
  type IfElse[B, T <: B, F <: B] <: B // def[B](t: => B, f => B): B
}

object TRUE extends BOOL{
  type &&[B <: BOOL] = B
  type ||[B <: BOOL] = TRUE.type
  type IfElse[B, T <: B, F <: B] = B
}
object FALSE extends BOOL{
  type &&[B <: BOOL] = FALSE.type
  type ||[B <: BOOL] = B
  type IfElse[B, T <: B, F <: B] = F
}

implicitly[(FALSE.&&[FALSE.type]) =:= FALSE.type] // false && false == true
implicitly[(FALSE.IfElse[Any, Int, String]) =:= String] // false && false == true