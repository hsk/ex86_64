package compiler

object expand {

  def apply(p:List[Any]):List[Any] = {
    p.map {
      case (n, a:List[String], bs:List[Any]) =>
        var l = argv(a, regs)
        bs.foreach{ b =>
          val (l2, id) = f(l, b)
          l = l2
        }
        (n, l.reverse)
    }
  }

  def argv(as:List[String], rs:List[Any]):List[Any] = {
    (as, rs) match {
      case (List(), rs) => List()
      case (a::as, r::rs) => ("movl", r, a)::argv(as, rs)
    }
  }

  val regs = List("%edi", "%esi", "%edx", "%ecx", "%r8d", "%r9d")
  def f(l:List[Any],e:Any):(List[Any],String) = e match {
    case ("add", a, b) =>
      val id = genid("ex_")
      val (la, a1) = f(l, a)
      val (lb, b1) = f(la, b)
      (("addl", a1, b1, id)::lb,id)
    case ("sub", a, b) =>
      val id = genid("ex_")
      val (la, a1) = f(l, a)
      val (lb, b1) = f(la, b)
      (("subl", a1, b1, id)::lb,id)
    case ("mov", a:String, id:String) => (("movl", a, id)::l, id)
    case ("ascii", a:String, id:String) => (("ascii", a, id)::l, id)
    case ("mov", a:Int, id:String) => (("movl", ("$"+a),id)::l, id)
    case ("mov", a@("ascii",_,_), id:String) =>
      val (l2, id1) = f(l, a)
      (("movadr", id1, id)::l2, id)
    case ("mov", a, id:String) =>
      val (l2, id1) = f(l, a)
      (("movl", id1, id)::l2, id)
    case ("call", a, b:List[Any]) =>
      var (la,ids) = b.foldLeft((l,List[String]())){
        case ((l,ids),b)=>
          val (l2,id) = f(l, b)
          (l2,id::ids)
      }
      (("call", a, ids)::la, "%eax")
    case ("if", a, b:List[Any], c:List[Any]) =>
      val (lb,bid) = b.foldLeft(List[Any](), null:String) {
        case ((l,a),b) => f(l, b)
      }
      val (lc,cid) = c.foldLeft(List[Any](), null:String) {
        case ((l,a),b) => f(l, b)
      }
      val (la,aid) = f(l,a)
      (("ifeq", aid, "$0",lb,lc)::la, null)
    case ("ret", e) =>
      val (l2, id) = f(l, e)
      (("ret", id)::l2, id)
    case id:String => (l, id)
    case e => (e::l, null)
  }

}
