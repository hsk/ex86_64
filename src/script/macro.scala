package script

import reader._
import scala.collection.mutable.{HashMap,Stack}

object macro {
  def apply(a:Any):Any = try {
    var env = new Env(null)
    env += ('macros -> new Stack[(Any,Any)]())
    env += ('print -> eval.print _ )
    env += ('void -> 'void)
    macro.expand(a,env)
  } catch {
    case e => println(e);-1
  }

  def qq(a:Any, e:Env):Any = {
    a match {
      case (a,b) => (qq(a,e),qq(b,e))
      case (a,b,c) => (qq(a,e),qq(b,e),qq(c,e))
      case ('qe,Symbol("{"),b,Symbol("}")) => b
      case ('q,Symbol("{"),b,Symbol("}")) => a
      case ('uq,Symbol("{"),b,Symbol("}")) => eval.eval(b,e)
      case (a,b,c,d) => (qq(a,e),qq(b,e),qq(c,e),qq(d,e))
      case (a,b,c,d,f) => (qq(a,e),qq(b,e),qq(c,e),qq(d,e),qq(f,e))
      case a => a
    }
  }

  def expand(a:Any, e:Env):Any = {
    (a,e) match {
      case macro(a,e) => a
      case ((('mac,Symbol("("),b,Symbol(")"),c),'@,d),e) =>
        e('macros).asInstanceOf[Stack[(Any,Any)]].push((b,c)); expand(d, e)
      case ((a,b),e) => (expand(a,e),expand(b,e))
      case ((a,b,c),e) => (expand(a,e),expand(b,e),expand(c,e))
      case ((a,b,c,d),e) => (expand(a,e),expand(b,e),expand(c,e),expand(d,e))
      case ((a,b,c,d,f),e) => (expand(a,e),expand(b,e),expand(c,e),expand(d,e),expand(f,e))
      case (a,e) => a
    }
  }

  def unapply(arg:(Any,Env)):Option[(Any,Env)]={
    arg match {
    case (a,e) =>
      for((prms,body) <- (e('macros).asInstanceOf[Stack[(Any,Any)]])) {
        var env = match5(prms,a,e)
        if(env != null) {
          return Some(eval.eval(body,env),e)
        }
      }
      None
    }
  }

  def match5(a:Any,b:Any,e:Env):Env = {
    var m = new Env(e)
    def mat(a:Any,b:Any):Boolean = {
      (a,b) match {
        case (Sym(n), b) => m += (Symbol(n) -> expand(b,e));true
        case ((a1,a2),(b1,b2)) => mat(a1,b1)&&mat(a2,b2)
        case ((a1,a2,a3),(b1,b2,b3)) => mat(a1,b1)&&mat(a2,b2)&&mat(a3,b3)
        case ((a1,a2,a3,a4),(b1,b2,b3,b4)) => mat(a1,b1)&&mat(a2,b2)&&mat(a3,b3)&&mat(a4,b4)
        case ((a1,a2,a3,a4,a5),(b1,b2,b3,b4,b5)) => mat(a1,b1)&&mat(a2,b2)&&mat(a3,b3)&&mat(a4,b4)&&mat(a5,b5)
        case (a,b) if(a==b)=> true
        case _ => false
      }
    }
    if(mat(a,b)) m
    else null
  }
}
