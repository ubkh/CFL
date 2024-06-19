// Compiler for the JVM
//====================== 

import os._

// lexer
import $file.cw02
import cw02._

// parser
import $file.cw03
import cw03._

val header = """
.class public XXX.XXX
.super java/lang/Object
"""

val start = """
.method public static main([Ljava/lang/String;)V
    .limit locals 200
    .limit stack 200
"""

val end = """
    return
.end method
"""

val writeInt = """
.method public static write(I)V
    .limit locals 1
    .limit stack 2
    getstatic java/lang/System/out Ljava/io/PrintStream;
    iload 0
    invokevirtual java/io/PrintStream/print(I)V
    return
.end method
"""

val writeStr = """
.method public static writes(Ljava/lang/String;)V
    .limit stack 2
    .limit locals 1
    getstatic java/lang/System/out Ljava/io/PrintStream;
    aload 0
    invokevirtual java/io/PrintStream/print(Ljava/lang/String;)V
    return
.end method
"""

val read = """
.method public static read()I
    .limit locals 10
    .limit stack 10
    ldc 0
    istore 1 ; this will hold our final integer
    Label1:
    getstatic java/lang/System/in Ljava/io/InputStream;
    invokevirtual java/io/InputStream/read()I
    istore 2
    iload 2
    ldc 10 ; test for the newline delimiter for Unix
    isub
    ifeq Label2
    iload 2
    ldc 13 ; test for the carriage -return in Windows
    isub
    ifeq Label2
    iload 2
    ldc 32 ; the space delimiter
    isub
    ifeq Label2
    iload 2
    ldc 48 ; we have our digit in ASCII, have to subtract it from 48
    isub
    ldc 10
    iload 1
    imul
    iadd
    istore 1
    goto Label1
    Label2:
    ; when we come here we have our integer computed
    ; in local variable 1
    iload 1
    ireturn
.end method
"""

// fresh variables
var counter = -1
var blockCounter = -1

def Fresh(x: String) = {
  counter += 1
  x ++ "_" ++ counter.toString()
}

def FreshBl() = {
  blockCounter += 1
  "Block_" ++ blockCounter.toString()
}

// for JVM instructions and labels
extension (sc: StringContext) {
    def i(args: Any*): String = "   " ++ sc.s(args:_*) ++ "\n"
    def l(args: Any*): String = sc.s(args:_*) ++ ":\n"
}


// environment where variables are stored
type Env = Map[String, Int]

// you can make changes to the arguments of the compile_*
// functions, but compile needs to take a block and a
// class_name as argument, and produce the j-file as 
// string.

def compile_aexp(a: AExp, env : Env) : String = a match {
  case Num(n) => i"ldc $n" // load constant
  case Var(x) => i"iload ${env(x)}" // load integer from local variable
  case Aop(op, a1, a2) => 
    compile_aexp(a1, env) + compile_aexp(a2, env) + (op match {
      case "+" => i"iadd"
      case "-" => i"isub"
      case "*" => i"imul"
      case "/" => i"idiv"
      case "%" => i"irem"
    })
}

def compile_bexp(b: BExp, env : Env, jmp: String) : String = b match {
  case True => ""
  case False => i"goto $jmp"
  case Bop(op, a1, a2) => 
    compile_aexp(a1, env) + compile_aexp(a2, env) + (op match {
      case "==" => i"if_icmpne $jmp"
      case "!=" => i"if_icmpeq $jmp"
      case "<" => i"if_icmpge $jmp"
      case ">" => i"if_icmple $jmp"
      case "<=" => i"if_icmpgt $jmp"
      case ">=" => i"if_icmplt $jmp"
    })
  case Lop(op, b1, b2) => op match {
    case "&&" => compile_bexp(b1, env, jmp) ++ compile_bexp(b2, env, jmp)
    case "||" => {
        val or_mid = Fresh("Or_mid")
        val or_end = Fresh("Or_end")

        compile_bexp(b1, env, or_mid) ++
        i"goto $or_end" ++
        l"$or_mid" ++
        compile_bexp(b2, env, jmp) ++
        l"$or_end"
    }
  }
}

def compile_stmt(s: Stmt, env: Env): (String, Env) = s match {
  case Skip => 
    ("", env)

  case Assign(x, a) => 
    val code = compile_aexp(a, env)
    val idx = env.getOrElse(x, env.size)
    (code + i"istore $idx", env + (x -> idx))

  case If(b, bl1, bl2) => 
    val jmpFalse = Fresh("If_else")
    val jmpEnd = Fresh("If_end")
    val codeB = compile_bexp(b, env, jmpFalse)
    val (code1, env1) = compile_block(bl1, env)
    val (code2, env2) = compile_block(bl2, env1)
    (codeB + code1 + i"goto $jmpEnd" + l"$jmpFalse" + code2 + l"$jmpEnd", env2)

  case While(b, bl) =>
    val startLoop = Fresh("While_begin")
    val endLoop = Fresh("While_end")
    val codeB = compile_bexp(b, env, endLoop)
    val block = FreshBl()
    val (codeBlock, env1) = compile_block(bl, env)
    (l"$startLoop" + codeB + codeBlock + i"goto $startLoop" + i"block" + l"$endLoop", env1)

  case Read(x) =>
    val idx = env.getOrElse(x, env.keys.size)
    ( i"invokestatic XXX/XXX/read()I" ++ i"istore ${idx}", env + (x -> idx))

  case WriteId(x) => 
    (i"iload ${env(x)}" ++ i"invokestatic XXX/XXX/write(I)V", env)

  case WriteString(x) => 
    (i"ldc $x" ++ i"invokestatic XXX/XXX/writes(Ljava/lang/String;)V", env)

  case For(s, from, to, bl) => {
    val startLoop = Fresh("For_begin")
    val endLoop   = Fresh("For_end")
    val idx = env.getOrElse(s, env.keys.size)
    val block = FreshBl()
    val (instrs1, env1) = compile_block(bl, env + (s -> idx))
    (
      compile_aexp(from, env1) ++            
      i"istore $idx" ++                   
      l"$startLoop" ++                     
      compile_bexp(Bop("<=", Var(s), to), env1, endLoop) ++
      instrs1 ++                           
      compile_stmt(Assign(s, Aop("+", Var(s), Num(1))), env1)._1 ++
      i"goto $startLoop" ++                 
      l"$block" ++                 
      l"$endLoop"
      ,
      env1
    )
  }

  case Break => {
    blockCounter -= 1
    val break = s"Block_$blockCounter"
    (i"goto $break", env)
  }
}

def compile_block(bl: Block, env: Env) : (String, Env) = bl match {
    case Nil => ("", env)
    case s::bl => {
        val block = FreshBl()
        val (instrs1, env1) = compile_stmt(s, env)
        val (instrs2, env2) = compile_block(bl, env1)
        (instrs1 ++ instrs2 ++ l"$block", env2)
    }
}

def compile(bl: Block, class_name: String) : String = {
    val env = Map[String, Int]() // Initialize the environment
    val (instrs, _) = compile_block(bl, env) // Compile the block

    (header + writeInt + writeStr + read + start + instrs + end)
    .replaceAllLiterally("XXX", class_name)
}

// Parse string then compile and return bytecode 
def parse_compile(program: String, class_name: String) : String =
    compile(Prg.parse_all(tokenise(program)).head, class_name)

def compile_run(name: String) : Unit = {
  val program = os.read(os.pwd / s"$name.while")
  write.over(pwd / s"$name.j", parse_compile(program, name))
  println(s"written to $name.j")
  os.proc("java", "-jar", "jasmin.jar", s"$name.j").call()
  os.proc("java", s"${name}/${name}").call(stdout = os.Inherit, stdin = os.Inherit)
}

// compile_run("fib")
// compile_run("factorial")
// compile_run("forloop1")
compile_run("forloop2")
// compile_run("break")