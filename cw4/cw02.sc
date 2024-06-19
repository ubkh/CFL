// CW 2
//======



// Rexp
abstract class Rexp
case object ZERO extends Rexp
case object ONE extends Rexp
case class CHAR(c: Char) extends Rexp
case class ALT(r1: Rexp, r2: Rexp) extends Rexp 
case class SEQ(r1: Rexp, r2: Rexp) extends Rexp 
case class STAR(r: Rexp) extends Rexp 
case class RECD(x: String, r: Rexp) extends Rexp

case class RANGE(s: Set[Char]) extends Rexp
case class PLUS(r: Rexp) extends Rexp
case class OPTIONAL(r: Rexp) extends Rexp
case class NTIMES(r: Rexp, n: Int) extends Rexp 

case class INTER(r1: Rexp, r2: Rexp) extends Rexp
case class UPTO(r: Rexp, n: Int) extends Rexp
case class FROM(r: Rexp, n: Int) extends Rexp
case class BETWEEN(r: Rexp, n: Int, m: Int) extends Rexp
case class NOT(r: Rexp) extends Rexp

case class CFUN(f: Char => Boolean) extends Rexp         // subsuming CHAR and RANGE

// Values, you might have to extend them 
// according to which values you want to create
abstract class Val
case object Empty extends Val
case class Chr(c: Char) extends Val
case class Sequ(v1: Val, v2: Val) extends Val
case class Left(v: Val) extends Val
case class Right(v: Val) extends Val
case class Stars(vs: List[Val]) extends Val
case class Rec(x: String, v: Val) extends Val
//
case class Plus(vs: List[Val]) extends Val
case class NTimes(vs: List[Val]) extends Val
case class Optl(v: Val) extends Val


// Convenience for typing
def charlist2rexp(s : List[Char]): Rexp = s match {
  case Nil => ONE
  case c::Nil => CHAR(c)
  case c::s => SEQ(CHAR(c), charlist2rexp(s))
}

implicit def string2rexp(s : String) : Rexp = 
  charlist2rexp(s.toList)

extension (r: Rexp) {
  def ~ (s: Rexp) = SEQ(r, s)
  def % = STAR(r)
  def | (s: Rexp) = ALT(r, s)
}


extension (s: String) {
  def | (r: Rexp) = ALT(s, r)
  def | (r: String) = ALT(s, r)
  def % = STAR(s)
  def ~ (r: Rexp) = SEQ(s, r)
  def ~ (r: String) = SEQ(s, r)
  def $ (r: Rexp) = RECD(s, r)
}

// nullable
def nullable(r: Rexp) : Boolean = r match {
  case ZERO => false
  case ONE => true
  case CHAR(_) => false
  case ALT(r1, r2) => nullable(r1) || nullable(r2)
  case SEQ(r1, r2) => nullable(r1) && nullable(r2)
  case STAR(_) => true
  // extensions
  case PLUS(r) => nullable(r)
  case NTIMES(r, n) => if (n == 0) true else nullable(r)
  case FROM(r, n) => if (n == 0) true else nullable(r)
  case BETWEEN(r, n, m) => if (n == 0) true else nullable(r)
  case RECD(_, r) => nullable(r)
  case OPTIONAL(_) => true
  case RANGE(_) => false
}

// der
def der(c: Char, r: Rexp) : Rexp = r match {
  case ZERO => ZERO
  case ONE => ZERO
  case CHAR(d) => if (c == d) ONE else ZERO
  case ALT(r1, r2) => ALT(der(c, r1), der(c, r2))
  case SEQ(r1, r2) => 
    if (nullable(r1)) ALT(SEQ(der(c, r1), r2), der(c, r2))
    else SEQ(der(c, r1), r2)
  case STAR(r) => SEQ(der(c, r), STAR(r))
  // extensions
  case PLUS(r) => SEQ(der(c, r), STAR(r))
  case OPTIONAL(r) => der(c, r)
  case UPTO(r, n) =>
    if (n == 0) ZERO else SEQ(der(c, r), UPTO(r, n - 1))
  case FROM(r, n) =>
    if (n == 0) SEQ(der(c, r), STAR(r)) else SEQ(der(c, r), FROM(r, n - 1))
  case BETWEEN(r, n, m) =>
    if (m == 0) ZERO else
    if (n == 0) SEQ(der(c, r), UPTO(r, m - 1))
    else SEQ(der(c, r), BETWEEN(r, n - 1, m - 1))
  case NOT(r) => NOT(der (c, r))
  case CFUN(f) => if (f(c)) ONE else ZERO
  case RECD(_, r) => der(c, r)
  case NTIMES(r, n) => if (n == 0) ZERO else SEQ(der(c, r), NTIMES(r, n - 1))
  case RANGE(s) => if (s.contains(c)) ONE else ZERO
}

// Flatten
def flatten(v: Val) : String = v match {
  case Empty => ""
  case Chr(c) => c.toString
  case Left(v) => flatten(v)
  case Right(v) => flatten(v)
  case Sequ(v1, v2) => flatten(v1) + flatten(v2)
  case Stars(vs) => vs.map(flatten).mkString
  // extensions
  case Rec(_, v) => flatten(v)
  case Plus(vs) => vs.map(flatten).mkString
  case NTimes(vs) => vs.map(flatten).mkString
  case Optl(v) => flatten(v)
}

// Env
def env(v: Val) : List[(String, String)] = v match {
  case Empty => Nil
  case Chr(c) => Nil
  case Left(v) => env(v)
  case Right(v) => env(v)
  case Sequ(v1, v2) => env(v1) ::: env(v2)
  case Stars(vs) => vs.flatMap(env)
  // extensions
  case Rec(x, v) => (x, flatten(v))::env(v)
  case Plus(vs) => vs.flatMap(env)
  case NTimes(vs) => vs.flatMap(env)
  case Optl(v) => env(v)
}

// Mkeps
def mkeps(r: Rexp) : Val = r match {
  case ONE => Empty
  case ALT(r1, r2) => 
    if (nullable(r1)) Left(mkeps(r1)) else Right(mkeps(r2))
  case SEQ(r1, r2) => Sequ(mkeps(r1), mkeps(r2))
  case STAR(r) => Stars(Nil)
  // extensions
  case PLUS(r) => Plus(List(mkeps(r)))
  case OPTIONAL(_) => Empty
  case NTIMES(r, 0) => NTimes(Nil)
  case NTIMES(r, n) => NTimes(List(mkeps(r)))
  case RECD(x, r) => Rec(x, mkeps(r))
}

// Inj
def inj(r: Rexp, c: Char, v: Val) : Val = (r, v) match {
  case (STAR(r), Sequ(v1, Stars(vs))) => Stars(inj(r, c, v1)::vs)
  case (SEQ(r1, r2), Sequ(v1, v2)) => Sequ(inj(r1, c, v1), v2)
  case (SEQ(r1, r2), Left(Sequ(v1, v2))) => Sequ(inj(r1, c, v1), v2)
  case (SEQ(r1, r2), Right(v2)) => Sequ(mkeps(r1), inj(r2, c, v2))
  case (ALT(r1, r2), Left(v1)) => Left(inj(r1, c, v1))
  case (ALT(r1, r2), Right(v2)) => Right(inj(r2, c, v2))
  case (CHAR(d), Empty) => Chr(c) 
  // extensions
  case (RANGE(s), Empty) => Chr(c)
  case (PLUS(r), Sequ(v1, Stars(vs))) => Plus(inj(r, c, v1)::vs)
  case (OPTIONAL(r), _) => Optl(inj(r, c, v))
  case (NTIMES(r, 0), _) => Empty
  case (NTIMES(r, n), Sequ(v1, NTimes(vs))) => NTimes(inj(r, c, v1)::vs)
  case (RECD(x, r1), _) => Rec(x, inj(r1, c, v))
}

// Rectification functions
def F_ID(v: Val): Val = v
def F_RIGHT(f: Val => Val) = (v:Val) => Right(f(v))
def F_LEFT(f: Val => Val) = (v:Val) => Left(f(v))
def F_ALT(f1: Val => Val, f2: Val => Val) = (v:Val) => v match {
  case Right(v) => Right(f2(v))
  case Left(v) => Left(f1(v))
}
def F_SEQ(f1: Val => Val, f2: Val => Val) = (v:Val) => v match {
  case Sequ(v1, v2) => Sequ(f1(v1), f2(v2))
}
def F_SEQ_Empty1(f1: Val => Val, f2: Val => Val) = 
  (v:Val) => Sequ(f1(Empty), f2(v))
def F_SEQ_Empty2(f1: Val => Val, f2: Val => Val) = 
  (v:Val) => Sequ(f1(v), f2(Empty))
def F_RECD(f: Val => Val) = (v:Val) => v match {
  case Rec(x, v) => Rec(x, f(v))
}
def F_ERROR(v: Val): Val = throw new Exception("error")

// Simp
def simp(r: Rexp): (Rexp, Val => Val) = r match {
  case ALT(r1, r2) => {
    val (r1s, f1s) = simp(r1)
    val (r2s, f2s) = simp(r2)
    (r1s, r2s) match {
      case (ZERO, _) => (r2s, F_RIGHT(f2s))
      case (_, ZERO) => (r1s, F_LEFT(f1s))
      case _ => if (r1s == r2s) (r1s, F_LEFT(f1s))
                else (ALT (r1s, r2s), F_ALT(f1s, f2s)) 
    }
  }
  case SEQ(r1, r2) => {
    val (r1s, f1s) = simp(r1)
    val (r2s, f2s) = simp(r2)
    (r1s, r2s) match {
      case (ZERO, _) => (ZERO, F_ERROR)
      case (_, ZERO) => (ZERO, F_ERROR)
      case (ONE, _) => (r2s, F_SEQ_Empty1(f1s, f2s))
      case (_, ONE) => (r1s, F_SEQ_Empty2(f1s, f2s))
      case _ => (SEQ(r1s,r2s), F_SEQ(f1s, f2s))
    }
  }
  case r => (r, F_ID)
}

// Lex
def lex_simp(r: Rexp, s: List[Char]) : Val = s match {
  case Nil => if (nullable(r)) mkeps(r) else 
    { throw new Exception("lexing error") } 
  case c::cs => {
    val (r_simp, f_simp) = simp(der(c, r))
    inj(r, c, f_simp(lex_simp(r_simp, cs)))
  }
}

def ders_simp(cs: List[Char], r: Rexp) : Rexp = cs match {
  case Nil => r
  case c::cs => ders_simp(cs, simp(der(c, r))._1)
} 

def lexing_simp(r: Rexp, s: String) = env(lex_simp(r, s.toList))

// Language specific code
val KEYWORD : Rexp = "while" | "if" | "then" | "else" | "do" | "for" | "to" | "true" | "false" | "read" | "write" | "skip" | "upto" | "break"
// operators are: +, -, *, %, /, ==, !=, >, <, <=, >=, :=, &&, ||
val OP : Rexp = "+" | "-" | "*" | "%" | "/" | "==" | "!=" | ">" | "<" | "<=" | ">=" | ":=" | "&&" | "||"
val LET: Rexp = RANGE(('a' to 'z').toSet) | RANGE(('A' to 'Z').toSet)
// symbols are letters plus the characters ., _, >, <, =, ;, , (comma), \ and :
val SYM: Rexp = RANGE((('A' to 'Z') ++ ('a' to 'z') ++ List('.', '_', '>', '<', '=', ';', ',', '\\', ':')).toSet)
//  parentheses are (, {, ) and }
val PARENS : Rexp = "("|"{"|")"|"}"
val SEMI : Rexp = ";"
// whitespaces are either " " (one or more) or \n or \t or \r
val WHITESPACE : Rexp = PLUS(" ") | "\n" | "\t" | "\r"

val DIGIT : Rexp = RANGE(('0' to '9').toSet)
val NUM : Rexp = DIGIT | (RANGE(('1' to '9').toSet) ~ DIGIT.%)

val ID : Rexp = LET ~ ("_" | LET | DIGIT).%
// strings are enclosed by double quotes, like "…", and consisting of symbols,
// digits, parentheses, whitespaces and \n (note the latter is not the escaped
// version but \ followed by n, otherwise we would not be able to indicate
// in our strings when to write a newline).
val STRING : Rexp = "\"" ~ (SYM | WHITESPACE | DIGIT | PARENS | "\\n" ).% ~ "\""
val EOL : Rexp = "\n" | "\r\n"
val COMMENT : Rexp = "//" ~ (SYM | " " | PARENS | DIGIT).% ~ EOL

val WHILE_REGS = (("k" $ KEYWORD) | 
                  ("o" $ OP) | 
                  ("str" $ STRING) |
                  ("p" $ PARENS) |
                  ("s" $ SEMI) | 
                  ("w" $ WHITESPACE) | 
                  ("i" $ ID) | 
                  ("n" $ NUM) |
		  ("c" $ COMMENT)).%

def esc(raw: String): String = {
  import scala.reflect.runtime.universe._
  Literal(Constant(raw)).toString
}

def escape(tks: List[(String, String)]) =
  tks.map{ case (s1, s2) => (s1, esc(s2))}

// Token
abstract class Token extends Serializable 
case class T_KEYWORD(s: String) extends Token
case class T_OP(s: String) extends Token
case class T_STRING(s: String) extends Token
case class T_PAREN(s: String) extends Token
case object T_SEMI extends Token
case class T_ID(s: String) extends Token
case class T_NUM(n: Int) extends Token
case class T_WHITESPACE(s: String) extends Token

val token : PartialFunction[(String, String), Token] = {
  case ("k", s) => T_KEYWORD(s)
  case ("o", s) => T_OP(s)
  case ("str", s) => T_STRING(s)
  case ("p", s) => T_PAREN(s)
  case ("s", _) => T_SEMI
  // case ("w", s) => T_WHITESPACE(s)
  case ("i", s) => T_ID(s)
  case ("n", s) => T_NUM(s.toInt)
}

// Tokenise
def tokenise(s: String) : List[Token] = 
  lexing_simp(WHILE_REGS, s).collect(token)


// Q2 Tests

// println(lex_simp(NTIMES("a", 3), "aaa".toList))
// println(lex_simp(NTIMES(("a" | ONE), 3), "aa".toList))

// lexing_simp(PLUS("a"), "aaa")
// lexing_simp(("i" $ ID), "myVar123")
// lexing_simp(OPTIONAL("a"), "a")

//println(tokenise("read n;"))
// println(tokenise("5 == 6"))


// Q3 Pr0grams

// println("Fib")
// println(tokenise(os.read(os.pwd / "fib.while")))

// println("Factors")
// println(tokenise(os.read(os.pwd / "factors.while")))

// println("Loops")
// println(tokenise(os.read(os.pwd / "loops.while")))

// println("Collatz")
// println(tokenise(os.read(os.pwd / "collatz.while")))

// println("Collatz2")
// println(tokenise(os.read(os.pwd / "collatz2.while")))

// println("Primezss")
// println(tokenise(os.read(os.pwd / "primes.while")))



