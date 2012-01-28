package test

import org.junit._
import Assert._
import java.io.FileInputStream
import reader.parse
import compiler._

class CompilerTest {

  @Test
  def test_asm {
    asm.open("a.txt")
    asm("test")
    asm.close()
    Assert.assertEquals("test\n",exec.readAll(new FileInputStream("a.txt")))
  }

  @Test
  def test_genid {
    assertEquals("a1",genid("a"))
    assertEquals("b2",genid("b"))
    assertEquals("a3",genid("a"))
  }

  @Test
  def test_emit {
    // 1を出力するプログラム
    emit("e.s", List(
        ("_main",List(
            ("movl", "$1", "%edi"), // スタックに1をつむ
            ("call", "_printInt",List()) // printInt関数を呼び出す
          ))
      ))
  }

  @Test
  def test_memAlloc {
    val prgs = List(
      ("_main",List(
          ("movl", "$5", "a"),
          ("call", "_printInt",List("a"))
        ))
    )
    val l = memAlloc(prgs)
    println("l="+l)
    emit("e.s",l)
    exec("gcc -m64 -o e e.s lib.c")
  }

  @Test
  def test_expand {
    val prg = List(
      ("_main", List(), List(
          ("mov", 10, "a"),
          ("mov", 1, "b"),
          ("mov", 2, "c"),
          ("mov", ("call", "_printInt", List(("add", "a", ("add", "b", "c")))),"d"),
          ("ret","d")
        )),
      ("_add", List("a","b"), List(
          ("ret", ("add","a","b"))
        ))
    )
    val p = expand(prg)
    println("p="+p)
    val m = memAlloc(p)
    println("m="+m)
    emit("e.s", m)
    exec("gcc -m64 -o e e.s lib.c")
  }

  @Test
  def test_setmem {
    val prg = List(
      ("_main",List(),List(
          ("call","_printInt",List(("call","_add",List(1,2,30))))
        )),
      ("_add", List("a","b","c"),List(
          ("ret",("add","a",("add","b","c")))
        ))

    )
    val s = setmem(prg)
    println("s="+s)
    val e = expand(s)
    val m = memAlloc(e)
    emit("e.s", m)
    exec("gcc -m64 -o e e.s lib.c")
  }

  @Test
  def test_st2ast {
    val st =
      (
        (
          'def,
          (
            ('main,Symbol("("),'void,Symbol(")")),
            Symbol("{"),
            (
              'printInt,
              Symbol("("),
              ('add,Symbol("("),(1,Symbol(","),(2,Symbol(","),3)),Symbol(")")),
              Symbol(")")
            ),
            Symbol("}")
          )
        ),
        '@,
        (
          'def,
          (
            ('add, Symbol("("),('a,Symbol(","),('b,Symbol(","),'c)),Symbol(")")),
            Symbol("{"),
            ('a,'+,('b,'+,'c)),
            Symbol("}")
          )
        )
      )
    val ast = st2ast(st)
    println("ast="+ast)
    val s = setmem(ast)
    val e = expand(s)
    val m = memAlloc(e)
    emit("e.s", m)
    exec("gcc -m64 -o e e.s lib.c")
  }

  @Test
  def test_emit_string {
    // 1を出力するプログラム
    emit("test_emit.s", List(
        ("_main",List(
            ("movl", "$1", "%edi"), // スタックに1をつむ
            ("ascii", "lb1", "test"), // スタックに1をつむ
            ("call", "_printInt",List()) // printInt関数を呼び出す
          ))
      ))
  }

  @Test
  def test_string {
    val prg = """def main(){ a="abc"; printInt(1)} """
    val st = parse(prg)
    val ast = st2ast(st)
    println("ast="+ast)
    val s = setmem(ast)
    println("s="+s)
    val e = expand(s)
    println("e="+e)
    val m = memAlloc(e)
    println("m="+m)
    emit("e_string.s", m)

  }

  @Test
  def test_string2 {
    main.main(Array[String]("test_str.dia"))
  }
}
