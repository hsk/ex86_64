package reader

case class Sym(s:String)

object parse {
  val infixs = Map[Any,Any](
    Symbol("(") -> ("p",170,Symbol(")")),
    Symbol("[") -> ("p",170,Symbol("]")),
    Symbol("{") -> ("p",170,Symbol("}")),
    Symbol(".") -> ("l", 170),
    '++ -> ("e", 160),
    '-- -> ("e", 160),
    '* -> ("l", 150),
    '/ -> ("l", 150),
    '% -> ("l", 150),

    '+ -> ("l", 140),
    '- -> ("l", 140),

    '<< -> ("l", 130),
    '>> -> ("l", 130),
    '>>> -> ("l", 130),
    '< -> ("l", 120),
    '<= -> ("l", 120),
    '> -> ("l", 120),
    '>= -> ("l", 120),
    'instanceof -> ("l", 120),
    'in -> ("l", 120),
    '== -> ("l", 110),
    '!= -> ("l", 110),
    '=== -> ("l", 100),
    '!== -> ("l", 100),
    '& -> ("l", 90),
    '^ -> ("l", 80),
    '| -> ("l", 70),
    '&& -> ("l", 60),
    '|| -> ("l", 50),
    Symbol(":") -> ("l", 50),
    '? -> ("r", 40),
    '= -> ("r", 40),
    '+= -> ("r", 40),
    '-= -> ("r", 40),
    '*= -> ("r", 40),
    '/= -> ("r", 40),
    '%= -> ("r", 40),
    '&= -> ("r", 40),
    '|= -> ("r", 40),
    '^= -> ("r", 40),
    '<<= -> ("r", 40),
    '>>= -> ("r", 40),
    '>>>= -> ("r", 40),
    Symbol(";") -> ("e2",30),
    Symbol(",") -> ("r", 20),
    'else ->("l2",10)
  )

  val prefixs = Map[Any,Any](
    Symbol("(") -> ("p", Symbol(")")),
    Symbol("[") -> ("p", Symbol("]")),
    Symbol("{") -> ("p", Symbol("}")),
    'new -> ("l", 160),
    '++ -> ("l", 160),
    '-- -> ("l", 160),
    '+ -> ("l", 160),
    '* -> ("l", 160),
    '& -> ("l", 160),
    '- -> ("l", 160),
    '~ -> ("l", 160),
    '! -> ("l", 160),
    'delete -> ("l", 160),
    'typeof -> ("l", 160),
    'break ->("l",160),
    'continue ->("l",160),
    'import ->("l",160),
    'def -> ("l", 20),
    'return ->("l",0),
    'mac->("st",Symbol("("),Symbol(")"),0),
    'if ->("st",Symbol("("),Symbol(")"),0),
    'for ->("st",Symbol("("),Symbol(")"),0),
    'while ->("st",Symbol("("),Symbol(")"),0),
    'do ->("st",Symbol("{"),Symbol("}"),0)
  )

  val comments = """(?s)^[\t\r\n ]*(#[^\r\n]*)(.*$)""".r
  val nums = """(?s)^[\t\r\n ]*([0-9]+)(.*$)""".r
  val ns = """(?s)^[\t\r\n ]*([a-zA-Z_][a-zA-Z_0-9]*|\*=|[\(\)\{\}\[\],;*]|[+\-/%&|\^~=!?:<>.]+|)(.*$)""".r
  val sym = """(?s)^[\t\r\n ]*'([^\(\)\[\]\{\}\s\;\:,]+)(.*$)""".r

  val strr = """(?s)^[\t\r\n ]*"((\\"|[^"])*)"(.*$)""".r
  val strrep = """(?s)(\\\\|\\t|\\n|\\r)""".r
  def apply(str:String):Any = {
    var src = str
    var token:Any = ""
    var ptoken:Any = ""
    def lex():Any = {
      ptoken = token
      src match {
        case comments(a,b) => src = b; lex()
        case nums(a,b) => token = a.toInt; src = b
        case sym(a,b) => token = Sym(a); src = b
        case strr(a,b,c) =>
          val rc = if(a==null) "" else a
          token = strrep.replaceAllIn(rc, a=>{
              (""+a) match {
                case """\\""" => """\\"""
                case """\t""" => "\t"
                case """\n""" => "\n"
                case """\r""" => "\r"
                case """\""" => """\\"""
                case a => a
              }
          })


          src = c
        case ns(a,b) => token = Symbol(a); src = b
      }
      ptoken
    }
    def eat(e:Any):Any = {
      if(lex() != e) {
        throw new Exception("syntax error. found unexpected token "+ptoken+ " expected "+e)
      }
      ptoken
    }
    lex()
    def exp(p:Int):Any = {
      if(token ==Symbol(")") || token == Symbol("}")|| token == Symbol("]")|| token==Symbol("")) return 'void
      def pr(t:Any):Any = {
        val op = t
        (if (prefixs.contains(op)) prefixs(op) else -1) match {
          case ("ep", _) => 'void
          case ("st", sp, ep, np:Int) =>
            eat(sp)
            val e = loop()
            eat(ep)
            (op, sp, e, ep, exp(np))
          case ("p",ep) => val e = loop(); (op,e,eat(ep))
          case ("l",np:Int) => (op, exp(np))
          case _ => op
        }
      }
      var t = pr(lex())
      def in(t:Any,f:Boolean):Any = {
        var op = (if (infixs.contains(token)) infixs(token) else -1)
        op match {
          case ("l",np:Int) if(f && np > p) => val op = lex(); in((t, op, exp(np)), true)
          case ("r",np:Int) if(f && np >= p) => val op = lex(); in((t, op, exp(np)), true)
          case ("e",np:Int) if(f && np >= p) => val op = lex(); in((t, op), true)
          case ("l2",np:Int) if(np > p) => val op = lex(); in((t, op, exp(np)), true)
          case ("e2",np:Int) if(np >= p) => val op = lex(); in((t, op), false)
          case ("p",np:Int,ep) if(f && np > p) => val sp = lex(); val e = loop(); in((t, sp, e, eat(ep)),true)
          case _ => t
        }
      }
      in(t,true)
    }
    def loop():Any = {
      val t = exp(0)
      token match {
        case Symbol("")|Symbol(")")|Symbol("]")|Symbol("}") => t
        case _ =>  (t, '@, loop())
      }
    }
    loop()
  }
}
