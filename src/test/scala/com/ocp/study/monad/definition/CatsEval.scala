package com.cop.study.monad.definition

import org.scalatest.{FunSpec, Matchers}
import cats.Eval
import cats._
import cats.implicits._
import scala.annotation.tailrec

class CatsEval extends FunSpec with Matchers {

  // the following is eager and memoized
  val x = {
    println("Computing X")
    math.random
  }

  // this maps to
  val now = Eval.now(math.random + 1000)

  // the following is lazy and not memoized
  def y = {
    println("Computing Y")
    math.random
  }

  // this maps to
  val later = Eval.later(math.random() + 2000)

  // this is lazy and memoized
  lazy val z = {
    println("Computing Z")
    math.random
  }

  // this maps to
  val always = Eval.always(math.random() + 3000)

  it("Can be used to make stuff stack safe") {

    def factorial(n: BigInt): BigInt = if (n == 1) n else n * factorial(n - 1)

    // right here we have a blowup
    // factorial(50000)

    // One option is we can write it to be tailcall recurisive
    @tailrec
    def factorial2(x: BigInt, acc: BigInt = 1): BigInt =
      if (x <= 1) acc else factorial2(x - 1, acc * x)

    factorial2(50000).toString().take(10) shouldBe "3347320509"

    // the other option is we can take the original and add evals to dever processing
    // the book says i can do Eval
    def factorial3(n: BigInt): Eval[BigInt] =
      if (n == 1) {
        Eval.now(n)
      } else {
        Eval.defer(factorial3(n - 1).map(_ * n))
      }

    factorial3(50000).value.toString().take(10) shouldBe "3347320509"
  }

  it("can make foldRight stack safe (Exercise 4.6.5") {

    def foldRight[A, B](as: List[A], acc: B)(fn: (A, B) => B): B =
      as match {
        case head :: tail =>
          fn(head, foldRight(tail, acc)(fn))
        case Nil =>
          acc
      }

    def safeFoldRight[A,B](as: List[A], acc: Eval[B])(fn: (A, Eval[B]) => Eval[B]): Eval[B] = as match {
      case Nil => acc
      case h :: t => Eval.defer(fn(h, safeFoldRight(t, acc)(fn)))
    }
  }
}
