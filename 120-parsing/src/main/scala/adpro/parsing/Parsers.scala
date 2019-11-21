// Andrzej Wąsowski, Advanced Programming

package adpro.parsing

// I used Scala's standard library lists, and scalacheck Props in this set,
// instead of those developed by the book.

import java.util.regex._
import scala.util.matching.Regex

// we need this for higher kinded polymorphism
import language.higherKinds
// we need this for introducing internal DSL syntax
import language.implicitConversions

trait Parsers[ParseError, Parser[+_]] { self =>

  def run[A] (p: Parser[A]) (input: String): Either[ParseError,A]
  implicit def char (c: Char): Parser[Char]
  implicit def string(s: String): Parser[String]
  implicit def operators[A] (p: Parser[A]): ParserOps[A] = ParserOps[A](p)
  def or[A] (s1: Parser[A], s2: =>Parser[A]): Parser[A]
  def listOfN[A] (n: Int, p: Parser[A]): Parser[List[A]]

  def map[A,B] (p: Parser[A]) (f: A => B): Parser[B]
  def many[A](p: Parser[A]): Parser[List[A]]
  def succeed[A](a: A): Parser[A] = string("") map (_ => a)
  def slice[A](p: Parser[A]): Parser[String]
  def product[A,B](p: Parser[A], p2: =>Parser[B]): Parser[(A,B)]
  def flatMap[A,B](p: Parser[A])(f: A => Parser[B]): Parser[B]

  implicit def regex(r: Regex): Parser[String]
  implicit def asStringParser[A] (a: A) (implicit f: A => Parser[String]): ParserOps[String] = ParserOps(f(a))

  case class ParserOps[A](p: Parser[A]) {

    def |[B>:A] (p2: Parser[B]): Parser[B] = self.or (p,p2)
    def or[B>:A] (p2: Parser[B]): Parser[B] = self.or (p,p2)
    def **[B] (p2: Parser[B]): Parser[(A,B)] = self.product (p,p2)
    def product[B] (p2: Parser[B]): Parser[(A,B)] = self.product (p,p2)

    def map[B] (f: A => B): Parser[B] = self.map (p) (f)
    def flatMap[B] (f: A => Parser[B]): Parser[B] = self.flatMap (p) (f)
    def many: Parser[List[A]] = self.many[A] (p)
    def slice: Parser[String] = self.slice (p)
  }

  object Laws {

    // Storing the laws in the trait -- the will be instantiated when we have
    // concrete implementation.  Still without a concrete implementation they
    // can be type checked, when we compile.  This tells us that the
    // construction of the laws is type-correct (the first step for them
    // passing).

    import org.scalacheck._
    import org.scalacheck.Prop._

    val runChar = Prop.forAll { (c: Char) => run(char(c))(c.toString) == Right(c) }
    val runString = Prop.forAll { (s: String) => run(string(s))(s) == Right(s) }

    val listOfN1 = Prop.protect (run(listOfN(3, "ab" | "cad"))("ababcad") == Right("ababcad"))
    val listOfN2 = Prop.protect (run(listOfN(3, "ab" | "cad"))("cadabab") == Right("cadabab"))
    val listOfN3 = Prop.protect (run(listOfN(3, "ab" | "cad"))("ababab") == Right("ababab"))
    val listOfN4 = Prop.protect (run(listOfN(3, "ab" | "cad"))("ababcad") == Right("ababcad"))
    val listOfN5 = Prop.protect (run(listOfN(3, "ab" | "cad"))("cadabab") == Right("cadabab"))
    val listOfN6 = Prop.protect (run(listOfN(3, "ab" | "cad"))("ababab") == Right("ababab"))

    def succeed[A] (a: A) = Prop.forAll { (s: String) => run(self.succeed(a))(s) == Right(a) }

    // Not planning to run this (would need equality on parsers), but can write
    // for typechecking:

    def mapStructurePreserving[A] (p: Parser[A]): Boolean =
      map(p)(a => a) == p
  }


  // Exercise 1

  // def manyA ...

  // Exercise 2

  def map2[A,B,C] (p: Parser[A], p2: Parser[B]) (f: (A,B) => C): Parser[C] = ???

  def many1[A] (p: Parser[A]): Parser[List[A]] = ???

  // Exercise 3

  // def digitTimesA  ...

  // Exercise 4

  // def product_[A,B] ...

  // def map2_ ...

  // Exercise 5

  // def map_ ...

}

trait JSON

object JSON {
  case object JNull extends JSON
  case class JNumber(get: Double) extends JSON
  case class JString(get: String) extends JSON
  case class JBool(get: Boolean) extends JSON
  case class JArray(get: IndexedSeq[JSON]) extends JSON
  case class JObject(get: Map[String, JSON]) extends JSON

  // The first argument is the parsers implementation P (that we don't have).
  // We write this code with only having the interface
  def jsonParser[ParseErr,Parser[+_]] (P: Parsers[ParseErr,Parser]): Parser[JSON] = {

    import P._
    val spaces = char (' ').many.slice

    ??? /* Exercise 6 */
  }
}
