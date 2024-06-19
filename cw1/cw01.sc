// CW 1

abstract class Rexp
case object ZERO extends Rexp
case object ONE extends Rexp
case class CHAR(c: Char) extends Rexp
case class ALT(r1: Rexp, r2: Rexp) extends Rexp 
case class SEQ(r1: Rexp, r2: Rexp) extends Rexp 
case class STAR(r: Rexp) extends Rexp 
// Extensions
case class RANGE(cs: Set[Char]) extends Rexp
case class PLUS(r: Rexp) extends Rexp
case class OPTIONAL(r: Rexp) extends Rexp
case class INTER(r1: Rexp, r2: Rexp) extends Rexp
case class NTIMES(r: Rexp, n: Int) extends Rexp
case class UPTO(r: Rexp, n: Int) extends Rexp
case class FROM(r: Rexp, n: Int) extends Rexp
case class BETWEEN(r: Rexp, n: Int, m: Int) extends Rexp
case class NOT(r: Rexp) extends Rexp

case class CFUN(f: Char => Boolean) extends Rexp         // subsuming CHAR and RANGE

// tests whether a regex can match the empty string
def nullable(r: Rexp): Boolean = r match {
  case ZERO => false
  case ONE => true
  case CHAR(_) => false
  case ALT(r1, r2) => nullable(r1) || nullable(r2)
  case SEQ(r1, r2) => nullable(r1) && nullable(r2)
  case STAR(_) => true
  // Extensions
  case RANGE(_) => false
  case PLUS(r1) => nullable(SEQ(r1, PLUS(r1)))
  case OPTIONAL(_) => true
  case INTER(r1, r2) => nullable(r1) && nullable(r2)
  case NTIMES(r1, 0) => true
  case NTIMES(r1, n) => nullable(SEQ(r1, NTIMES(r1, n-1)))
  case UPTO(_, 0) => true // When n=0 pattern is nullable
  case UPTO(_, n) if n >= 0 => true
  case FROM(_, 0) => true
  case FROM(r1, n) => nullable(SEQ(r1, FROM(r1, n-1)))
  case BETWEEN(_, 0, _) => true
  case BETWEEN(_, n, _) => false
  case NOT(r) => !nullable(r)

  case CFUN(_) => false
}

// the derivative of a regular expression w.r.t. a character
def der(c: Char, r: Rexp): Rexp = r match {
  case ZERO => ZERO
  case ONE => ZERO
  case CHAR(d) => if (c == d) ONE else ZERO
  case ALT(r1, r2) => ALT(der(c, r1), der(c, r2))
  case SEQ(r1, r2) => 
    if (nullable(r1)) ALT(SEQ(der(c, r1), r2), der(c, r2))
    else SEQ(der(c, r1), r2)
  case STAR(r1) => SEQ(der(c, r1), STAR(r1))
  // Extensions
  case RANGE(chars) => if (chars.contains(c)) ONE else ZERO
  // case PLUS(r1) => SEQ(der(c, r1), STAR(r1))
  case PLUS(r1) => FROM(der(c, r1), 1)
  case OPTIONAL(r1) => ALT(der(c, r1), ONE)
  case INTER(r1, r2) => INTER(der(c, r1), der(c, r2))
  case NTIMES(r1, 0) => ZERO
  case NTIMES(r1, n) => SEQ(der(c, r1), NTIMES(r1, n-1))
  case UPTO(_, 0) => ZERO
  case UPTO(r1, n) => ALT(SEQ(der(c, r1), UPTO(r1, n-1)), ONE)
  // case FROM(r, 0) => der(c, STAR(r))
  case FROM(r, 0) => ONE
  case FROM(r1, n) if n > 0 => SEQ(der(c, r1), FROM(r1, n-1))
  // case BETWEEN(r1, n, m) if n > 0 => ALT(SEQ(der(c, r1), BETWEEN(r1, n-1, m-1)), BETWEEN(r1, n, m-1))
  // case BETWEEN(r1, 0, m) if m > 0 => ALT(SEQ(der(c, r1), BETWEEN(r1, 0, m-1)), BETWEEN(r1, 0, m-1))
  // case BETWEEN(_, 0, 0) => ZERO
  case BETWEEN(r1, n, m) if n > 0 => 
    SEQ(der(c, r1), BETWEEN(r1, n-1, m-1))
  case BETWEEN(r1, 0, m) if m > 0 => 
    ALT(SEQ(der(c, r1), BETWEEN(r1, 0, m-1)), BETWEEN(r1, 0, m-1))
  case BETWEEN(_, 0, 0) => ZERO

  case NOT(CHAR(d)) => NOT(der(c, CHAR(d)))
  case NOT(r1) => NOT(der(c, r1))

  case CFUN(f) => if (f(c)) ONE else ZERO
}

def c(ch: Char): Rexp = CFUN(_ == ch)

def range(chars: Char*): Rexp = CFUN(chars.contains)

def ALL: Rexp = CFUN(_ => true)

// simplification
// def simp(r: Rexp): Rexp = r match {
//   case ALT(r1, r2) => (simp(r1), simp(r2)) match {
//     case (ZERO, r2s) => r2s
//     case (r1s, ZERO) => r1s
//     case (r1s, r2s) => if (r1s == r2s) r1s else ALT (r1s, r2s)
//   }
//   case SEQ(r1, r2) =>  (simp(r1), simp(r2)) match {
//     case (ZERO, _) => ZERO
//     case (_, ZERO) => ZERO
//     case (ONE, r2s) => r2s
//     case (r1s, ONE) => r1s
//     case (r1s, r2s) => SEQ(r1s, r2s)
//   }
//   case r => r
// }
def simp(r: Rexp): Rexp = r match {
  case ALT(r1, r2) => (simp(r1), simp(r2)) match {
    case (ZERO, r2s) => r2s
    case (r1s, ZERO) => r1s
    case (r1s, r2s) => if (r1s == r2s) r1s else ALT(r1s, r2s)
  }
  case SEQ(r1, r2) =>  (simp(r1), simp(r2)) match {
    case (ZERO, _) => ZERO
    case (_, ZERO) => ZERO
    case (ONE, r2s) => r2s
    case (r1s, ONE) => r1s
    case (r1s, r2s) => SEQ(r1s, r2s)
  }
  case INTER(r1, r2) => (simp(r1), simp(r2)) match {
    case (ZERO, r2s) => r2s
    case (r1s, ZERO) => r1s
    case (r1s, r2s) => if (r1s == r2s) r1s else INTER(r1s, r2s)
  }
  case r => r
}


// Derivative w.r.t. a string (iterates der)
// def ders(s: List[Char], r: Rexp): Rexp = s match {
//   case Nil => r
//   case c::s => ders(s, simp(der(c, r)))
// }

def ders(s: List[Char], r: Rexp, failed: Boolean = false): Rexp = {
  s match {
    case Nil => if (failed) ZERO else r
    case c::cs if failed => ZERO
    case c::cs => 
      val nextRexp = simp(der(c, r))
      nextRexp match {
        case NOT(ONE) => ders(cs, ZERO, true)
        case NOT(ZERO) => ders(cs, ONE, true)
        case _ => ders(cs, nextRexp)
      }
  }
}

// Matcher function
def matcher(r: Rexp, s: String): Boolean = 
  nullable(ders(s.toList, r))






// Test Cases
//============

// Q3
// val testPatterns = List(
//   OPTIONAL(CHAR('a')),
//   NOT(CHAR('a')),
//   NTIMES(CHAR('a'), 3),
//   NTIMES(OPTIONAL(CHAR('a')), 3),
//   UPTO(CHAR('a'), 3),
//   UPTO(OPTIONAL(CHAR('a')), 3),
//   BETWEEN(CHAR('a'), 3, 5),
//   BETWEEN(OPTIONAL(CHAR('a')), 3, 5),
//   NTIMES(CHAR('a'), 0)
// )

// val testStrings = List("", "a", "aa", "aaa", "aaaa", "aaaaa")

// for (s <- testStrings) {
//   println(s"Testing string: $s")
//   for (pattern <- testPatterns) {
//     val matches = matcher(pattern, s)
//     println(s"Pattern $pattern matches string $s: $matches")
//   }
// }

//Q4
// val isVowel = CFUN(c => "AEIOUaeiou".contains(c))

// assert(der('A', isVowel) == ONE)
// assert(der('b', isVowel) == ZERO)

// val isDigit = CFUN(c => "0123456789".contains(c))

// assert(der('3', isDigit) == ONE)
// assert(der('a', isDigit) == ZERO)

// val isLower = CFUN(c => c.isLower)

// assert(der('a', isLower) == ONE)
// assert(der('A', isLower) == ZERO)


// some syntactic convenience

def charlist2rexp(s: List[Char]) : Rexp = s match {
 case Nil => ONE
 case c::Nil => CHAR(c)
 case c::s => SEQ(CHAR(c), charlist2rexp(s))
}
implicit def string2rexp(s: String) : Rexp = charlist2rexp(s.toList)

extension (r: Rexp) {
 def ~ (s: Rexp) = SEQ(r, s)
 def % = STAR(r)
}


// Question 5

// println("EMAIL:")
val LOWERCASE = ('a' to 'z').toSet
val DIGITS = ('0' to '9').toSet
val SYMBOLS1 = ("_.-").toSet
val SYMBOLS2 = (".-").toSet
val EMAIL = { PLUS(CFUN(LOWERCASE | DIGITS | SYMBOLS1)) ~ "@" ~ 
             PLUS(CFUN(LOWERCASE | DIGITS | SYMBOLS2)) ~ "." ~
             BETWEEN(CFUN(LOWERCASE | Set('.')), 2, 6) }

val my_email = "ubayd.khan@kcl.ac.uk"

// println(EMAIL);
// println(matcher(EMAIL, my_email))
// println(ders(my_email.toList,EMAIL))


// Question 6
// def Q6test(description: String, condition: => Boolean): Unit = {
//     if (!condition) {
//         println(s"Test failed: $description")
//     } else {
//         println(s"Test passed: $description")
//     }
// }

// Q6test("SEQ with ZERO simplification (left)", simp(SEQ(CHAR('a'), ZERO)) == ZERO)
// Q6test("SEQ with ZERO simplification (right)", simp(SEQ(ZERO, CHAR('a'))) == ZERO)

// Q6test("SEQ with ONE simplification (left)", simp(SEQ(CHAR('a'), ONE)) == CHAR('a'))
// Q6test("SEQ with ONE simplification (right)", simp(SEQ(ONE, CHAR('a'))) == CHAR('a'))

// Q6test("INTER simplification (left)", simp(INTER(CHAR('a'), ZERO)) == CHAR('a'))
// Q6test("INTER simplification (middle)", simp(INTER(CHAR('a'), CHAR('a'))) == CHAR('a'))
// Q6test("INTER simplification (right)", simp(INTER(ZERO, CHAR('a'))) == CHAR('a'))


val COMMENT = """/*""" ~ (NOT(ALL.% ~ """*/""" ~ ALL.%)) ~ """*/"""

// println(matcher(COMMENT, """/**/"""))
// println(matcher(COMMENT, """/*foobar*/"""))
// println(matcher(COMMENT, """/*test*/test*/"""))
// println(matcher(COMMENT, """/*test/*test*/"""))

// Question 7
 
val r1 = SEQ(SEQ(CHAR('a'), CHAR('a')), CHAR('a'))
val r2 = SEQ(BETWEEN(CHAR('a'), 19, 19), OPTIONAL(CHAR('a')))
val s1 = "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
val s2 = "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
val s3 = "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"

// for (s <- List(s1,s2,s3)) {
//  println(matcher(PLUS(PLUS(r1)), s))
//  println(matcher(PLUS(PLUS(r2)), s))
// }
 

