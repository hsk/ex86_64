package compiler

object emit {

  def apply(filename:String, ls:List[Any]) {
    asm.open(filename)
    ls.foreach {
      case (name:String,body:List[Any]) =>

        asm(".cstring")
        def s(e:Any):Any = e match {
          case ("ascii", a, name) =>
            asm(name+":")
            asm("\t.ascii \""+a+"\\0\"")
          case _ =>
        }
        body.foreach(s)
        asm(".text")

        asm(".globl "+name)
        asm(name+":")
        asm("\tpushq\t%rbp")
        asm("\tmovq\t%rsp, %rbp")
        def f(e:Any):Any = e match {
          case ("movl",a,b) => asm("movl "+a+", %eax");asm("movl %eax, "+b)
          case ("subq",a,b) => asm("subq "+a+", "+b)
          case ("addl",a,b,c) =>
            asm("movl "+a+", %eax")
            asm("addl "+b+", %eax")
            asm("movl %eax, "+c)
          case ("subl",a,b,c) =>
            asm("movl "+a+", %eax")
            asm("subl "+b+", %eax")
            asm("movl %eax, "+c)
          case ("call", n, b:List[Any]) => prms(b, regs); asm("call "+n)
          case ("ret", a) =>
            asm("movl "+a+", %eax")
            asm("leave")
            asm("ret")
          case ("ifeq", a, b, c:List[Any], d:List[Any]) =>
            val id_else = genid("id_else")
            val id_cont = genid("id_cont")
            asm("movl "+a+", %eax")
            asm("cmpl "+b+", %eax")
            asm("jne "+id_else)
            c.foreach(f)
            asm(id_else+":")
            asm("jmp "+id_cont)
            c.foreach(f)
            asm(id_cont+":")
          case ("ascii",a,b)=>
          case ("movadr", a, b)=>
            asm("leaq "+a+"(%rip), %rax")
            asm("movq %rax, "+b)
        }
        body.foreach(f)
        asm("\tleave")
        asm("\tret")
    }
    asm.close()
  }
  
  val regs = List("%edi","%esi", "%edx")
  def prms(ps:List[Any],rs:List[Any]) {
    (ps,rs) match {
      case (List(),_) =>
      case (p::ps,r::rs) =>
        asm("movl "+p+", "+r)
        prms(ps, rs)
    }
  }
}
