//  grammar:
//
//  S  -: E$
//  E  -: T E2
//  E2 -: '|' E3
//  E2 -: NIL
//  E3 -: T E2
//  T  -: F T2
//  T2 -: F T2
//  T2 -: NIL
//  F  -: A F2
//  F2 -: '?' F2
//  F2 -: NIL
//  A  -: C
//  A  -: '(' A2
//  A2 -: E ')'

import scala.io.StdIn.readLine
abstract class S
case class E(left: T, right: Option[E2]) extends S
case class E2(left: E3) extends S
case class E3(left: T, right: Option[E2]) extends S
case class T(left: F, right: Option[T2]) extends S
case class T2(left: F, right: Option[T2]) extends S
case class F(left: A, right: Option[F2]) extends S
case class F2(left: Option[F2]) extends S
abstract class A extends S
case class A2(left: E) extends A
case class C(left: Char) extends A

class RecursiveDescent(input: String) { // parsing
  var inputIndex = 0

  def peek(): Char = input.charAt(inputIndex)
  def inc(): Unit = inputIndex += 1

  def parseS(): E = parseE()
  def parseE(): E = E(parseT(), parseE2())

  def parseE2() : Option[E2] = {
    if (inputIndex < input.length() && peek() == ')') {
      inc()
      None
    }
    else if (inputIndex < input.length && peek() == '|') {
      inc()
      Some(E2(parseE3))
    }
    else
      None
  }

  def parseE3(): E3 = E3(parseT, parseE2)

  def parseT(): T = T(parseF, parseT2())

  def parseT2(): Option[T2] = {
    if (inputIndex < input.length && peek() != '|' && peek() != '?' && peek() != ')')
        Some(T2(parseF, parseT2()))
    else
      None
  }

  def parseF(): F = F(parseA, parseF2())

  def parseF2(): Option[F2] = {
    if (inputIndex < input.length && peek() == '?') {
      inc()
      Some(F2(parseF2))
    }
    else
      None
  }

  def parseA(): A = {
    if (peek() == '(') {
      inc()
      A2(parseE())
    }

    else {
      inc()
      C(input.charAt(inputIndex - 1))
    }
  }

  def parseC(): Option[C] = {
    if (inputIndex < input.length && peek().isLetterOrDigit || peek() == ' ' || peek() == '.') {
        inc()
        Some(C(input.charAt(inputIndex - 1)))
      }
    else
      None
  }
}

object Main {
  def main(args: Array[String]) {
    val rd = new RecursiveDescent(readLine("pattern? "))
    val exp:S = rd.parseE()
    var str = readLine("string? ")

    while (str != "exit") {
      val matches: Boolean = evaluate(str, exp)
      if (matches) {
        println("match")
      } else println("no match")
      str = readLine("string? ")
    }
  }

  def evaluate(input: String, tree: S): Boolean = {
    var index = 0

    def descent(s: S): Boolean = { // I would've loved to catch multiple matches in one case statement but it isn't letting me :(
      val last = index

      s match { // recursively travel through tree returning booleans 
        case s:E =>
          val l: Boolean = descent(s.left)
          s.right match {
            case Some(r) =>
              if (!l) { // '|' checking
                index = last
                descent(r)
              }
              else true // left was true
            case None => l
          }

        case s: E2 => descent(s.left) // is either E3 or None

        case s: E3 =>
          val l: Boolean = descent(s.left)
          s.right match {
            case Some(r) =>
              if (!l) {
                index = last
                descent(r)
              }
              else true
            case None => l
          }

        case s: T =>
          s.right match {
            case Some(r) =>
              if (s.left.right.nonEmpty && descent(r)) // '?' checking
                true
              else {
                index = last
                descent(s.left) && descent(r)
              }
            case None => descent(s.left)
          }

        case s: T2 =>
          s.right match {
            case Some(r) =>
              if (s.left.right.nonEmpty && descent(r))
                true
              else {
                index = last
                descent(s.left) && descent(r)
              }
            case None => descent(s.left)
          }

        case s: F =>
          s.right match {
            case Some(_) =>
              val l = descent(s.left)
              if (!l)
                index = last
              true
            case None => descent(s.left)
          }

        case s: A2 => descent(s.left) // check E on left

        case s: C => // '.' can be used to represent any char
          if (index < input.length && (s.left == input.charAt(index) || s.left == '.')) {
            index += 1 // only place we increment, as string is only characters (as opposed to parsing)
            true
          }
          else false
      }
    }

    val result = descent(tree)
    if (index < input.length) // ensure input is long enough
      false
    else result
  }
}