package script

import reader._
import scala.collection.mutable.{HashMap,Stack}

object eval {

  def apply(s:String):Any = try {
    var env = new Env(null)
    env += ('macros -> new Stack[(Any,Any)]())
    env += ('print -> print _ )
    env += ('void -> 'void)
    var exp = parse(s)
    eval(exp,env)
  } catch {
    case e => println(e);-1
  }
  def print(a:Any):Any = {
    println(a)
    a
  }
  case class Fun(prms:Any,body:Any,e:Env) {
    override def  toString():String = {
      return "Fun("+prms+","+body+")"
    }
  }

  def append(a:Any,op:Symbol, b:Any):Any = {
    a match {
      case (a, `op` ,as) => (a,op,append(as,op,b))
      case a => (a,op,b)
    }
  }
  // c like expression evaluator
  def eval(a:Any, e:Env):Any = {
    def listToArray(a:Any,c:java.util.Vector[Any]):Unit = a match {
      case Symbol(a) => c.add(a)
      case (Symbol(a),Symbol(","),b) => c.add(a); listToArray(b, c)
      case a => c.add(a)
    }
    def getTypes(a:Any,c:java.util.Vector[java.lang.Class[_]]):Unit = a match {
      case (a,Symbol(","),b) => c.add(a.asInstanceOf[AnyRef].getClass()); getTypes(b, c)
      case a => c.add(a.asInstanceOf[AnyRef].getClass()); c
    }
    def ev(a:Any, b:Any,e:Env, f:(Any,Any)=>Any):Any = (eval(a,e),eval(b,e)) match {
      case (a:Int, b) => f(a,b.asInstanceOf[Int])
      case (a, b:Int) => f(a.asInstanceOf[Int],b)
      case (a:Float, b) => f(a,b.asInstanceOf[Float])
      case (a, b:Float) => f(a.asInstanceOf[Float],b)
      case (a:Double, b) => f(a,b.asInstanceOf[Double])
      case (a, b:Double) => f(a.asInstanceOf[Double],b)
      case (a:AnyRef,b) => f(a.toString(),b.toString())
      case (a,b:AnyRef) => f(a.toString(),b.toString())
      case (a,b) => f(a.asInstanceOf[Int], b.asInstanceOf[Int])
    }

    //println("eval("+a+")");
    a match {
      case (('new,(a,Symbol("("),b,Symbol(")")))) =>
        def className(a:Any):String = a match {
          case Symbol(a) => a
          case (a,Symbol("."),b) => className(a)+"."+className(b)
        }
        var name = className(a)
        val loader = ClassLoader.getSystemClassLoader()
        

        b match {
          case 'void =>
            val rc = loader.loadClass(name).newInstance()
            return rc;
          case b =>
            var constructors = loader.loadClass(name).getConstructors()
            var p = new java.util.Vector[Any]()
            listToArray(b, p)
            var params = p.toArray()
            for(i <- 0 until constructors.size) {
              constructors(i)match {
              case con if(con.getParameterTypes().size==params.size) =>
                val rc = con.newInstance(params:_*)
                println("rc="+rc)
                return rc
              case _ =>
              }
            }
        }
        
      case (a,'++) => val r = e(a); e += (a -> (r.asInstanceOf[Int] + 1)); r
      case (a,'--) => val r = e(a); e += (a -> (r.asInstanceOf[Int] - 1)); r
      case (a,'*,b) =>
        ev(a,b,e,{ case(a:Double,b:Double)=>a*b case(a:Float,b:Float)=>a*b case(a:Long,b:Long)=>a*b case(a:Int,b:Int)=>a*b})
      case (a,'/,b) =>
        ev(a,b,e,{ case(a:Double,b:Double)=>a/b case(a:Float,b:Float)=>a/b case(a:Long,b:Long)=>a/b case(a:Int,b:Int)=>a/b})
      case (a,'%,b) =>
        ev(a,b,e,{ case(a:Double,b:Double)=>a%b case(a:Float,b:Float)=>a%b case(a:Long,b:Long)=>a%b case(a:Int,b:Int)=>a%b})
      case (a,'+,b) =>
        ev(a,b,e,{ case(a:String,b:String)=>a+b case(a:Double,b:Double)=>a+b case(a:Float,b:Float)=>a+b case(a:Long,b:Long)=>a+b case(a:Int,b:Int)=>a+b})
      case (a,'-,b) =>
        ev(a,b,e,{ case(a:Double,b:Double)=>a-b case(a:Float,b:Float)=>a-b case(a:Int,b:Int)=>a-b})
      case (a,'<<,b) => eval(a, e).asInstanceOf[Int] << eval(b, e).asInstanceOf[Int]
      case (a,'>>,b) => eval(a, e).asInstanceOf[Int] >> eval(b, e).asInstanceOf[Int]
      case (a,'>>>,b) => eval(a, e).asInstanceOf[Int] >>> eval(b, e).asInstanceOf[Int]
      case (a,'<,b) => (eval(a,e),eval(b,e)) match {
          case (a:Int, b:Int) => if(a < b) -1 else 0
          case (a1, b1) => throw new Error("unknown "+a +"< "+ b +" -> " + a1 + "< "+ b1)
        }
      case (a,'<=,b) => (eval(a,e),eval(b,e)) match {
          case (a:Int, b:Int) => if(a <= b) -1 else 0
          case (a1, b1) => throw new Error("unknown "+a +"<= "+ b +" -> " + a1 + "<= "+ b1)
        }
      case (a,'>,b) => (eval(a,e),eval(b,e)) match {
          case (a:Int, b:Int) => if(a > b) -1 else 0
          case (a1, b1) => throw new Error("unknown "+a +"> "+ b +" -> " + a1 + "> "+ b1)
        }
      case (a,'>=,b) => (eval(a,e),eval(b,e)) match {
          case (a:Int, b:Int) => if(a >= b) -1 else 0
          case (a1, b1) => throw new Error("unknown "+a +">= "+ b +" -> " + a1 + ">= "+ b1)
        }
//      case (a,'instanceof,b) => eval(a,e).asInstanceOf[eval(b,e)]
      case (a,'==,b) => if(eval(a,e) == eval(b,e))-1 else 0
      case (a,'!=,b) => if(eval(a,e) != eval(b,e))-1 else 0
      case (a,'===,b) => if(eval(a,e) == eval(b,e))-1 else 0
      case (a,'!==,b) => if(eval(a,e) != eval(b,e))-1 else 0
      case (a,'&,b) => eval(a, e).asInstanceOf[Int] & eval(b, e).asInstanceOf[Int]
      case (a,'|,b) => eval(a, e).asInstanceOf[Int] | eval(b, e).asInstanceOf[Int]
      case (a,'&& ,b) => if((eval(a, e).asInstanceOf[Int]!=0) && (eval(b, e).asInstanceOf[Int]!=0)) -1 else 0
      case (a,'|| ,b) => if((eval(a, e).asInstanceOf[Int]!=0) || (eval(b, e).asInstanceOf[Int]!=0)) -1 else 0
      case (a,'=,b) => val r = eval(b,e);  e += (a -> r); r
      case (a,'+=,b) => val r = e(a).asInstanceOf[Int] + eval(b,e).asInstanceOf[Int]; e += (a -> r); r
      case (a,'-=,b) => val r = e(a).asInstanceOf[Int] - eval(b,e).asInstanceOf[Int]; e += (a -> r); r
      case (a,'*=,b) => val r = e(a).asInstanceOf[Int] * eval(b,e).asInstanceOf[Int]; e += (a -> r); r
      case (a,'/=,b) => val r = e(a).asInstanceOf[Int] / eval(b,e).asInstanceOf[Int]; e += (a -> r); r
      case (a,'%=,b) => val r = e(a).asInstanceOf[Int] % eval(b,e).asInstanceOf[Int]; e += (a -> r); r
      case (a,'&=,b) => val r = e(a).asInstanceOf[Int] & eval(b,e).asInstanceOf[Int]; e += (a -> r); r
      case (a,'|=,b) => val r = e(a).asInstanceOf[Int] | eval(b,e).asInstanceOf[Int]; e += (a -> r); r
      case (a,'^=,b) => val r = e(a).asInstanceOf[Int] ^ eval(b,e).asInstanceOf[Int]; e += (a -> r); r
      case (a,'<<=,b) => val r = e(a).asInstanceOf[Int] << eval(b,e).asInstanceOf[Int]; e += (a -> r); r
      case (a,'>>=,b) => val r = e(a).asInstanceOf[Int] >> eval(b,e).asInstanceOf[Int]; e += (a -> r); r
      case (a,'>>>=,b) => val r = e(a).asInstanceOf[Int] >>> eval(b,e).asInstanceOf[Int]; e += (a -> r); r

      case ('++,a) => val r = e(a).asInstanceOf[Int] + 1; e += (a -> r); r
      case ('--,a) => val r = e(a).asInstanceOf[Int] - 1; e += (a -> r); r
      case ('+,a)   => (eval(a, e).asInstanceOf[Int])
      case ('-,a)   => -(eval(a, e).asInstanceOf[Int])
      case ('~,a)   => ~(eval(a, e).asInstanceOf[Int])
      case ('!,a)   => if(eval(a, e)==0) -1 else 0

      case (a,'@, b) => eval(a, e); eval(b, e)
//      case ('print,Symbol("("), a,Symbol(")")) => val r = eval(a, e); println(r);r
      case ('eval,Symbol("("),b,Symbol(")")) => eval(b,e)
      case ((a,Symbol("."),Symbol(b)),Symbol("("),c,Symbol(")")) =>
        val obj = eval(a,e)
        val clazz = obj.asInstanceOf[AnyRef].getClass()
        //val v = new java.util.Vector[java.lang.Class[_]]()
        //getTypes(c, v)
        //val va = v.toArray().asInstanceOf[Array[Class[_]]]
        val ms = clazz.getMethods();

        var p = new java.util.Vector[Any]()
        listToArray(c, p)
        var params:Array[AnyRef] = p.toArray()
        for(i <- 0 until ms.size) {
          ms(i)match {
          case con if(con.getName()==b && con.getParameterTypes().size==params.size) =>
            println("params="+params)
              return con.invoke(obj, params:_*)
          case _ =>
          }
        }
      case (a,Symbol("("),b,Symbol(")")) =>
        (eval(a,e),eval(b,e)) match {
          case (a:Int,b:Int) => a + b
          case (a:(Any=>Any), b) =>
            a(eval(b, e))
          case (Fun(a, body, e),b) =>
            // arguments bind
            var e2 = new Env(e)
            def bind(a:Any, b:Any) {
              (a, b) match {
                case ((a,Symbol(","),as),(b,Symbol(","),bs)) =>
                  e2.e += (a -> b); bind(as, bs)
                case ((a,Symbol(","),as), b) =>
                  e2.e += (a -> b)
                case (a,b) => e2.e += (a -> b)
              }
            }
            bind(a, b)
            eval(body, e2)
//          case (a:Any, b) =>
//            a.getMethod("main", classOf[Array[String]]).invoke(cls, Array[String]())

          case (_,_)=> throw new Error("unknown function:"+a);
        }

      case ((Symbol("("),b,Symbol(")")),Symbol("{"),c,Symbol("}")) =>
        Fun(b,c,e)
      case ((a,Symbol("("),b,Symbol(")")),Symbol("{"),c,Symbol("}")) =>
        eval((a,Symbol("("),append(b,Symbol(","),Fun('void,c,e)),Symbol(")")), e)
      case ('q,Symbol("{"),b,Symbol("}")) => b
//      case ('qq,Symbol("{"),b,Symbol("}")) => macro.qq(b, e)
//      case ('qqq,Symbol("{"),b,Symbol("}")) => macro.qq(eval(b,e), e)
      case (a,Symbol("{"),b,Symbol("}")) => eval(a, e).asInstanceOf[Int] * eval(b, e).asInstanceOf[Int]
      case (a,Symbol("["),b,Symbol("]")) => eval(a, e).asInstanceOf[Int] - eval(b, e).asInstanceOf[Int]
      case (Symbol("("),a,Symbol(")")) => eval(a, e)
      case (Symbol("{"),a,Symbol("}")) => eval(a, e)
      case (a,Symbol(","),b) => (eval(a, e),Symbol(","),eval(b,e))
      case (Symbol("["),a,Symbol("]")) => (Symbol("["), eval(a, e), Symbol("]"))
      case (a, Symbol(";"))  => eval(a, e)
      case ('if,Symbol("("),a,Symbol(")"),(b,'else,c)) => if(eval(a,e) != 0) eval(b, e) else eval(c, e)
      case ('if,Symbol("("),a,Symbol(")"), b) => if(eval(a, e) != 0) eval(b, e) else 0
      case ('def,((Symbol("("),a,Symbol(")")),Symbol("{"),b,Symbol("}"))) => val rc = Fun(a,b,e); rc
      case ('def,((name,Symbol("("),a,Symbol(")")),Symbol("{"),b,Symbol("}"))) => val f = Fun(a,b,e); e += (name -> f); f
      case a:Fun => a
      case a:Symbol => e(a)
      case Sym(a) => Symbol(a)
      case a:Int => a
      case a:String => a
      case a => throw new Error("runtime error " + a)
    }}

}

