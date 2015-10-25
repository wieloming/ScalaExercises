trait CanFoo[A] { def foos(x: A): String}
case class Wrapper(wrapped: String)

//typeClass:
implicit object WrapperCanFoo extends CanFoo[Wrapper] {
  def foos(x: Wrapper) = x.wrapped
}

//usage:
def foo[A](thing: A)(implicit evidence: CanFoo[A]) = evidence.foos(thing)
//usage with syntactic sugar:
def foo2[A: CanFoo](thing: A) = implicitly[CanFoo[A]].foos(thing)

//even more sugar with companion object:
object CanFoo {def apply[A: CanFoo]: CanFoo[A] = implicitly}
def foo3[A: CanFoo](thing: A) = CanFoo[A].foos(thing)

foo(Wrapper("hi"))
foo2(Wrapper("hi"))
foo3(Wrapper("hi"))

