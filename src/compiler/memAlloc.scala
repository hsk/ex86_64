package compiler

object memAlloc {
  var m:Map[String,String] = null
  def apply(ls:List[Any]):List[Any] = ls.map {
    case (n:String,ls:List[Any])=>
      counter = 0
      m = Map()
      val ll = ls.map(g)
      val size = ((15-counter)/16)*16
      (n,("subq","$"+size,"%rsp")::ll)
  }

  def g(l:Any):Any = l match {
    case ("movl", a, b) => ("movl", adr(a), adr(b))
    case ("movadr", a, b) => ("movadr", a, adrn(b,8))
    case ("addl", a, b, c) => ("addl", adr(a), adr(b),adr(c))
    case ("subl", a, b, c) => ("subl", adr(a), adr(b),adr(c))
    case ("call", a, b:List[Any]) => ("call", a, b.map(adr))
    case ("ret", a) => ("ret", adr(a))
    case ("ifeq", a, b, c:List[Any], d:List[Any]) =>
      ("ifeq", adr(a),adr(b), c.map(adr), d.map(adr))
    case ("ascii", a, b) => ("ascii", a, b)
  }

  var counter = 0 
  def adr(a:Any):Any = adrn(a, 4)
  def adrn(a:Any,size:Int):Any = a match {
    case a:String if(m.contains(a))=> m(a)
    case a:String if(a.substring(0,1)=="%" || a.substring(0,1)=="$") => a 
    case a:String => counter -= size; val n = counter + "(%rbp)"; m = m + (a -> n); n
    case a => a
  }
}

