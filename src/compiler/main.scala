package compiler

import java.io._
import reader._
import script._

object main {
  def main(argv:Array[String]) {
    val prg = exec.readAll(new FileInputStream(argv(0)))
    val st = parse(prg)
    val st2 = macro(st)
    val ast = st2ast(st2)
    val s = setmem(ast)
    val e = expand(s)
    val m = memAlloc(e)
    emit("e.s", m)
    exec("gcc -m64 -o e e.s lib.c")
    exec("./e")
  }
}
