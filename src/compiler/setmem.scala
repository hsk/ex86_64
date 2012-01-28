package compiler

object setmem {
  var ls:List[Any] = List()

  def apply(e:List[Any]):List[Any] = e.map {
    case (n:String, a:List[String], b:List[Any]) =>
      ls = List()
      val b2 = b.map(f)
      (n, a, ls:::b2)
  }

  def f(e:Any):Any = e match {
    case ("mov", a, b) => ("mov", f(a), f(b))
    case ("ret", a) => ("ret", a)
    case ("add", a, b) => ("add", f(a), f(b))
    case ("sub", a, b) => ("sub", f(a), f(b))
    case ("call", a, b:List[Any]) => ("call", a, b.map(f))
    case a:Int =>
      val id = genid("s_")
      ls = ("mov",a,id)::ls
      id
    case a => a
  }

}
