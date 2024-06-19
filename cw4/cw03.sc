// CW3
//=====
//
// The main idea is to extend the parser form the lectures 
// (which processes strings) to a parser that processes
// tokens. For this you need to use the lexer from CW2 and
// possibly adjust the lexing regular expressions accordingly.

import $file.cw02
import cw02._

case class ~[+A, +B](x: A, y: B)

// parser combinators

type IsSeq[I] = I => Seq[_]

abstract class Parser[I, T](using is: IsSeq[I])  {
  def parse(in: I): Set[(T, I)]  

  def parse_all(in: I) : Set[T] =
    for ((hd, tl) <- parse(in); 
        if is(tl).isEmpty) yield hd
}

// parser combinators


// alternative parser
class AltParser[I : IsSeq, T](p: => Parser[I, T], 
                              q: => Parser[I, T]) extends Parser[I, T] {
  def parse(in: I) = p.parse(in) ++ q.parse(in)   
}

// sequence parser
class SeqParser[I: IsSeq, T, S](p: => Parser[I, T], 
                                q: => Parser[I, S]) extends Parser[I, ~[T, S]] {
  def parse(in: I) = 
    for ((hd1, tl1) <- p.parse(in); 
         (hd2, tl2) <- q.parse(tl1)) yield (new ~(hd1, hd2), tl2)
}

// map parser
class MapParser[I : IsSeq, T, S](p: => Parser[I, T], 
                         f: T => S) extends Parser[I, S] {
  def parse(in: I) = for ((hd, tl) <- p.parse(in)) yield (f(hd), tl)
}

// some convenient syntax for parser combinators
extension [I: IsSeq, T](p: Parser[I, T]) {
  def ||(q : => Parser[I, T]) = new AltParser[I, T](p, q)
  def ~[S] (q : => Parser[I, S]) = new SeqParser[I, T, S](p, q)
  def map[S](f: => T => S) = new MapParser[I, T, S](p, f)
}

def ListParser[I, T, S](p: => Parser[I, T], q: => Parser[I, S])(using is: I => Seq[_]): Parser[I, List[T]] = {
  (p ~ q ~ ListParser(p, q)).map{ case (x:T) ~ (y:S) ~ (z:List[T]) => x :: z } ||
  (p.map[List[T]]{s => List(s)})
}

case class TknParser(tok: Token) extends Parser[List[Token], Token] {
  def parse(ts: List[Token]) = ts match {
    case t::ts if (t == tok) => Set((t, ts)) 
    case _ => Set ()
  }
}

implicit def token2tparser(t: Token) : Parser[List[Token], Token] = TknParser(t)

extension (t: Token) {
  def || (q : => Parser[List[Token], Token]) = new AltParser[List[Token], Token](t, q)
  def map[S] (f: => Token => S) = new MapParser[List[Token], Token, S](t, f)
  def ~[S](q : => Parser[List[Token], S]) = new SeqParser[List[Token], Token, S](t, q)
}

// Abstract Syntax Trees
abstract class Stmt // statements

abstract class AExp { // arithmetic expressions
  override def toString: String
}

case class Var(s: String) extends AExp {
  override def toString: String = s"Var($s)"
}

case class Num(i: Int) extends AExp {
  override def toString: String = s"Num($i)"
}

case class Aop(op: String, left: AExp, right: AExp) extends AExp {
  override def toString: String = s"Aop($op, $left, $right)"
}

abstract class BExp { // boolean expressions
  override def toString: String
}

case object True extends BExp {
  override def toString: String = "True"
}

case object False extends BExp {
  override def toString: String = "False"
}

case class Bop(op: String, left: AExp, right: AExp) extends BExp {
  override def toString: String = s"Bop($op, $left, $right)"
}

// for logical operations: and, or
case class Lop(op: String, left: BExp, right: BExp) extends BExp {
  override def toString: String = s"Lop($op, $left, $right)"
}

type Block = List[Stmt]

case object Break extends Stmt
case object Skip extends Stmt
case class If(a: BExp, bl1: Block, bl2: Block) extends Stmt
case class While(b: BExp, bl: Block) extends Stmt
case class Assign(s: String, a: AExp) extends Stmt
case class Read(s: String) extends Stmt
case class WriteId(s: String) extends Stmt  // for printing values of variables
case class WriteString(s: String) extends Stmt  // for printing words
case class For(s: String, from: AExp, to: AExp, bl: Block) extends Stmt

// Atomic parser for strings
case object StrParser extends Parser[List[Token], String] {
  def parse(tokens: List[Token]) = tokens match {
    case T_STRING(s)::rest => Set((s, rest))
    case _ => Set()
  }
}

// Atomic parser for identifiers
case object IdParser extends Parser[List[Token], String] {
  def parse(tokens: List[Token]) = tokens match {
    case T_ID(s)::rest => Set((s, rest)) // Map the string to a Var node
    case _ => Set()
  }
}

// Atomic parser for numbers
case object NumParser extends Parser[List[Token], Int] {
  def parse(tokens: List[Token]) = tokens match {
    case T_NUM(n)::rest => Set((n, rest)) // Map the integer to a Num node
    case _ => Set()
  }
}

// arithmetic expressions
lazy val AExp: Parser[List[Token], AExp] = 
  (Te ~ T_OP("+") ~ AExp).map[AExp]{ case x ~ _ ~ z => Aop("+", x, z) } ||
  (Te ~ T_OP("-") ~ AExp).map[AExp]{ case x ~ _ ~ z => Aop("-", x, z) } || Te
lazy val Te: Parser[List[Token], AExp] = 
  (Fa ~ T_OP("*") ~ Te).map[AExp]{ case x ~ _ ~ z => Aop("*", x, z) } || 
  (Fa ~ T_OP("/") ~ Te).map[AExp]{ case x ~ _ ~ z => Aop("/", x, z) } || 
  (Fa ~ T_OP("%") ~ Te).map[AExp]{ case x ~ _ ~ z => Aop("%", x, z) } || Fa  
lazy val Fa: Parser[List[Token], AExp] = 
   (T_PAREN("(") ~ AExp ~ T_PAREN(")")).map{ case _ ~ y ~ _ => y } || 
   IdParser.map(Var(_)) || 
   NumParser.map(Num(_))

// bool expressions
lazy val BExp: Parser[List[Token], BExp] = 
  BExpOr
lazy val BExpOr: Parser[List[Token], BExp] = 
  (BExpAnd ~ T_OP("||") ~ BExpOr).map[BExp]{ case x ~ _ ~ z => Lop("||", x, z) } || 
  BExpAnd
lazy val BExpAnd: Parser[List[Token], BExp] = 
  (BExpComp ~ T_OP("&&") ~ BExpAnd).map[BExp]{ case x ~ _ ~ z => Lop("&&", x, z) } ||
  BExpComp
lazy val BExpComp: Parser[List[Token], BExp] = 
  (AExp ~ T_OP("==") ~ AExp).map[BExp]{ case x ~ _ ~ z => Bop("==", x, z) } || 
  (AExp ~ T_OP("!=") ~ AExp).map[BExp]{ case x ~ _ ~ z => Bop("!=", x, z) } || 
  (AExp ~ T_OP("<") ~ AExp).map[BExp]{ case x ~ _ ~ z => Bop("<", x, z) } || 
  (AExp ~ T_OP(">") ~ AExp).map[BExp]{ case x ~ _ ~ z => Bop(">", x, z) } ||
  (AExp ~ T_OP("<=") ~ AExp).map[BExp]{ case x ~ _ ~ z => Bop("<=", x, z) } ||
  (AExp ~ T_OP("=>") ~ AExp).map[BExp]{ case x ~ _ ~ z => Bop("=>", x, z) } ||
  BExpTerm
lazy val BExpTerm: Parser[List[Token], BExp] = 
  (T_KEYWORD("true").map[BExp]{ _ => True }) || 
  (T_KEYWORD("false").map[BExp]{ _ => False }) ||
  (T_PAREN("(") ~ BExp ~ T_PAREN(")")).map[BExp]{ case _ ~ x ~ _ => x }

// statements
lazy val Stmt: Parser[List[Token], Stmt] =
   (T_KEYWORD("skip").map[Stmt]{_ => Skip }) ||
   (IdParser ~ T_OP(":=") ~ AExp).map[Stmt]{ case x ~ _ ~ z => Assign(x, z) } ||
   (T_KEYWORD("write") ~ T_PAREN("(") ~ IdParser ~ T_PAREN(")")).map[Stmt]{ case _ ~ _ ~ y ~ _ => WriteId(y) } ||
   (T_KEYWORD("write") ~ T_PAREN("(") ~ StrParser ~ T_PAREN(")")).map[Stmt]{ case _ ~ _ ~ y ~ _ => WriteString(y) } ||
   (T_KEYWORD("write") ~ IdParser ).map[Stmt]{ case _ ~ y => WriteId(y) } ||
   (T_KEYWORD("write") ~ StrParser).map[Stmt]{ case _ ~ y => WriteString(y) } ||
   (T_KEYWORD("read") ~ IdParser).map[Stmt]{ case _ ~ y => Read(y) } ||
   (T_KEYWORD("if") ~ BExp ~ T_KEYWORD("then") ~ Block ~ T_KEYWORD("else") ~ Block)
     .map[Stmt]{ case _ ~ y ~ _ ~ u ~ _ ~ w => If(y, u, w) } ||
   (T_KEYWORD("if") ~ T_PAREN("(") ~ BExp ~ T_PAREN(")") ~ T_PAREN("{") ~ Block ~ T_PAREN("}") ~ T_KEYWORD("else") ~ T_PAREN("{") ~ Block ~ T_PAREN("}"))
     .map[Stmt]{ case _ ~ _ ~ y ~ _ ~ _ ~ u ~ _ ~ _ ~ _ ~ w ~ _ => If(y, u, w) } ||
   (T_KEYWORD("while") ~ BExp ~ T_KEYWORD("do") ~ Block).map[Stmt]{ case _ ~ y ~ _ ~ w => While(y, w) } ||
   (T_KEYWORD("while") ~ T_PAREN("(") ~ BExp ~ T_PAREN(")") ~ Block).map[Stmt]{ case _ ~ _ ~ y ~ _ ~ w => While(y, w) } ||
   (T_KEYWORD("for") ~ IdParser ~ T_OP(":=") ~ AExp ~ T_KEYWORD("upto") ~ AExp ~ T_KEYWORD("do") ~ Block)
      .map[Stmt]{ case _ ~ x ~ _ ~ y ~ _ ~ z ~ _ ~ w => For(x, y, z, w) } ||
   (T_KEYWORD("break").map[Stmt]{ _ => Break })
 
lazy val Stmts: Parser[List[Token], Block] =
  (Stmt ~ T_SEMI ~ Stmts).map[Block]{ case x ~ _ ~ z => x :: z } ||
  (Stmt.map[Block]{ s => List(s) }) ||
  (Stmt ~ T_SEMI).map[Block]{ case s ~ _ => List(s) } ||
  (Stmt ~ T_KEYWORD("break") ~ Stmts).map[Block]{ case _ ~ _ ~ z => z } 

// blocks
lazy val Block: Parser[List[Token], Block] =
  ((T_PAREN("{") ~ Stmts ~ T_PAREN("}")).map{ case _ ~ y ~ _ => y } || 
   (Stmt.map(s => List(s))))

// program
val Prg = Stmts


// Interpreter
//=============

// Import needed to take int as input from the user
import scala.io.StdIn.readInt

type Env = Map[String, Int]

def eval_aexp(a: AExp, env: Env) : Int = a match {
  case Num(i) => i
  case Var(s) => env(s)
  case Aop("+", a1, a2) => eval_aexp(a1, env) + eval_aexp(a2, env)
  case Aop("-", a1, a2) => eval_aexp(a1, env) - eval_aexp(a2, env)
  case Aop("*", a1, a2) => eval_aexp(a1, env) * eval_aexp(a2, env)
  case Aop("/", a1, a2) => eval_aexp(a1, env) / eval_aexp(a2, env)
  case Aop("%", a1, a2) => eval_aexp(a1, env) % eval_aexp(a2, env)
}

def eval_bexp(b: BExp, env: Env): Boolean = b match {
  case True => true
  case False => false
  case Bop("==", a1, a2) => eval_aexp(a1, env) == eval_aexp(a2, env)
  case Bop("!=", a1, a2) => eval_aexp(a1, env) != eval_aexp(a2, env)
  case Bop("<", a1, a2) => eval_aexp(a1, env) < eval_aexp(a2, env)
  case Bop(">", a1, a2) => eval_aexp(a1, env) > eval_aexp(a2, env)
  case Bop("<=", a1, a2) => eval_aexp(a1, env) <= eval_aexp(a2, env)
  case Bop(">=", a1, a2) => eval_aexp(a1, env) >= eval_aexp(a2, env)
  case Lop("&&", b1, b2) => eval_bexp(b1, env) && eval_bexp(b2, env)
  case Lop("||", b1, b2) => eval_bexp(b1, env) || eval_bexp(b2, env)
}

def eval_stmt(s: Stmt, env: Env): Env = s match {
  case Skip => env
  case Break => env
  case Assign(x, a) => env + (x -> eval_aexp(a, env))
  case If(b, bl1, bl2) => if (eval_bexp(b, env)) eval_bl(bl1, env) else eval_bl(bl2, env)
  case While(b, bl) => 
    if (eval_bexp(b, env)) eval_stmt(While(b, bl), eval_bl(bl, env))
    else env
  case Read(x) =>
    // println(s"Enter value for $x: ")
    val value = readInt()
    env + (x -> value)
  case WriteId(x) =>
    println(env.getOrElse(x, 0))
    env
  case WriteString(str) => { print(str.replaceAll("\"", "").replace("\\n","")) ; env }
  case For(x, from, to, bl) => {
    val fromVal = eval_aexp(from, env)
    val toVal = eval_aexp(to, env)
    var loopEnv = env + (x -> fromVal) 

    while (loopEnv(x) <= toVal) {
      loopEnv = eval_bl(bl, loopEnv) 
      loopEnv = loopEnv + (x -> (loopEnv(x) + 1)) 
    }
    
    loopEnv 
  }

}

def eval_bl(bl: Block, env: Env) : Env = bl match {
  case Nil => env
  case s::bl => eval_bl(bl, eval_stmt(s, env))
}

def eval(bl: Block) : Env = eval_bl(bl, Map())


// AST TESTING

// AEXP
// val testExpr = "3 * (2 + 4)"
// val expected = 18  

// val tokens = tokenise(testExpr)
// val parseResult = AExp.parse(tokens)

// parseResult.headOption match {
//   case Some((ast, _)) => 
//     println("AExp AST: " + ast.toString)
//   case None => 
//     println("Parsing failed")
// }

// BEXP
// val testExpr2 = "x == 5 && y > 7"
// val tokens2 = tokenise(testExpr2)
// val parseResult2 = BExp.parse(tokens2)

// parseResult2.headOption match {
//   case Some((ast, _)) => 
//     println("BExp AST: " + ast.toString)
//   case None => 
//     println("Parsing failed")
// }

// Others
// val skipTest = "skip"
// val skipTokens = tokenise(skipTest)
// val skipAst = Stmt.parse(skipTokens).headOption.getOrElse(throw new Exception("Parsing failed"))._1
// assert(skipAst == Skip, "AST mismatch for Skip")

// val ifTest = "if (x == 5) { skip } else { skip }"
// val ifTokens = tokenise(ifTest)
// val ifAst = Stmt.parse(ifTokens).headOption.getOrElse(throw new Exception("Parsing failed"))._1
// assert(ifAst == If(Bop("==", Var("x"), Num(5)), List(Skip), List(Skip)), "AST mismatch for If")

// val whileTest = "while (x < 5) { skip }"
// val whileTokens = tokenise(whileTest)
// val whileAst = Stmt.parse(whileTokens).headOption.getOrElse(throw new Exception("Parsing failed"))._1
// assert(whileAst == While(Bop("<", Var("x"), Num(5)), List(Skip)), "AST mismatch for While")

// val assignTest = "x := 5"
// val assignTokens = tokenise(assignTest)
// val assignAst = Stmt.parse(assignTokens).headOption.getOrElse(throw new Exception("Parsing failed"))._1
// assert(assignAst == Assign("x", Num(5)), "AST mismatch for Assign")

// val q2 = "if (a < b) then skip else a := a * b + 1"
// val q2Tokens = tokenise(q2)
// val q2AST = Stmt.parse_all(q2Tokens).headOption.getOrElse(throw new Exception("Parsing failed"))
// println(Stmt.parse_all(q2Tokens).headOption)
// println(q2AST)

// external programs
// println("Fib")
// val fibTokens = tokenise(os.read(os.pwd / "fib.while"))
// val fibAST = Prg.parse_all(fibTokens).headOption.getOrElse(throw new Exception("Parsing failed"))
// println(fibAST)

// println("Factors")
// val factorsTokens = tokenise(os.read(os.pwd / "factors.while"))
// val factorsAST = Prg.parse_all(factorsTokens).headOption.getOrElse(throw new Exception("Parsing failed"))
// println(factorsAST)

// println("Loops")
// val loopsTokens = tokenise(os.read(os.pwd / "loops.while"))
// val loopsAST = Prg.parse_all(loopsTokens).headOption.getOrElse(throw new Exception("Parsing failed"))
// println(loopsAST)

// println("Collatz")
// val collatzTokens = tokenise(os.read(os.pwd / "collatz.while"))
// val collatzAST = Prg.parse_all(collatzTokens).headOption.getOrElse(throw new Exception("Parsing failed"))
// println(collatzAST)

// println("Collatz 2")
// val collatz2Tokens = tokenise(os.read(os.pwd / "collatz2.while"))
// val collatz2AST = Prg.parse_all(collatz2Tokens).headOption.getOrElse(throw new Exception("Parsing failed"))
// println(collatz2AST)

// println("Primes")
// val primesTokens = tokenise(os.read(os.pwd / "primes.while"))
// val primesAST = Prg.parse_all(primesTokens).headOption.getOrElse(throw new Exception("Parsing failed"))
//println(primesAST)

// println("For Loop")
// val forTokens = tokenise(os.read(os.pwd / "forloop1.while"))
// println(forTokens)
// println(Prg.parse_all(forTokens))
// val forAST = Prg.parse_all(forTokens).headOption.getOrElse(throw new Exception("Parsing failed"))
// println(forAST)

// val for2 = "for a := 2 upto 4 do { write a }"
// val for2Tokens = tokenise(for2)
// println(for2Tokens)
// val for2AST = Prg.parse_all(for2Tokens)
// println(for2AST)

// val for3 = "for a := 2 upto 4 do { if a > 3 then break else skip; }"
// val for3Tokens = tokenise(for3)
// println(for3Tokens)
// val for3AST = Prg.parse_all(for3Tokens)
// println(for3AST)

// EVAL TESTING
//println(eval(primesAST))
// sample output for primes.while
/*
2
3
5
7
11
13
17
19
23
29
31
37
41
43
47
53
59
61
67
71
73
79
83
89
97
Map(end -> 100, n -> 100, f -> 4, tmp -> 1)
*/

// println(eval(fibAST))
// println(eval(factorsAST))
// println(eval(loopsAST))
// println(eval(collatzAST))
// println(eval(collatz2AST))


// TIME MEASUREMENT - works, but commented out to avoid automated testing failing

// def time_needed[T](n: Int, code: => T): Double = {
//   val start = System.nanoTime()
//   for (_ <- 0 until n) code
//   val end = System.nanoTime()
//   (end - start) / (n * 1.0e9)
// }

// val startValues = List(100, 500, 700, 1000, 1500, 2000, 2500, 3000)
// startValues.foreach { startValue =>
//   val template = os.read(os.pwd / "loops.while")
//   val modified = template.replace("start := 1000", s"start := $startValue")

//   val parsed = Prg.parse_all(tokenise(modified)).head
//   val time = time_needed(1, eval(parsed)) 
//   println(s"Execution time for start = $startValue: $time seconds")
// }

