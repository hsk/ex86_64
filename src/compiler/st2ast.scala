package compiler

object st2ast {

  def apply(st:Any):List[Any] = st match {
    case (a,'@,b) => f(a)::apply(b)
    case a => List(f(a))
  }

  def f(fn:Any):Any = fn match {
    case ('def,((Symbol(n),Symbol("("),a,Symbol(")")),Symbol("{"),b, Symbol("}"))) =>
      ("_"+n, params(a), bodys(b))
  }

  def params(e:Any):List[Any] = e match {
    case (a,Symbol(","),b) => params(a):::params(b)
    case 'void=>List()
    case Symbol(a) => List(a)
  }
  def fargs(e:Any):List[Any] = e match {
    case (a,Symbol(","),b) => fargs(a):::fargs(b)
    case a => List(exp(a))
  }

  def exp(e:Any):Any = e match {
    case (Symbol("{"),b,Symbol("}")) => bodys(b)
    case (Symbol("("),b,Symbol(")")) => exp(b)
    case (Symbol(a),Symbol("("),b,Symbol(")")) => ("call","_"+a,fargs(b))
    case (a,'=,b) => ("mov", exp(b), exp(a))
    case (a,'+,b) => ("add",exp(a), exp(b))
    case (a,'-,b) => ("sub",exp(a), exp(b))
    case ('return, a) => ("ret", exp(a))
    case (a,Symbol(";")) => exp(a)
    case a:Int => a
    case Symbol(a) => a
    case a:String => ("ascii",a,genid("str_"))
    case a => a
  }
  def bodys(e:Any):List[Any] = e match {
    case (a,'@,b) => bodys(a):::bodys(b)
    case a =>
      exp(a) match {
        case e:List[Any] => e
        case a => List(a)
      }
  }
}

