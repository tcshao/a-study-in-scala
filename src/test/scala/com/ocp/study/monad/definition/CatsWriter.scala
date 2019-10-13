package com.ocp.study.monad.definition

import org.scalatest.{FunSpec, Matchers}
import cats.data.Writer
import cats.instances.vector._ // for Monoid
import cats.syntax.applicative._ // for pure
import cats.syntax.writer._ // for tell

class CatsWriter extends FunSpec with Matchers {

  type Logged[A] = Writer[Vector[String], A]

  it("looks like this") {

    Writer(
      Vector(
        "It was the best of times",
        "it was the worst of times"
      ),
      1859
    )

    val x = 123.pure[Logged] // type is WriterT((Vector(),123))

    // the opposite of this is a Writer with no value
    Vector("one", "two", "three").tell

    println(x)
  }

  it("has some further examples") {

    val writer1 = for {
      a <- 10.pure[Logged]
      _ <- Vector("a1", "b1", "c1").tell
      b <- 32.writer(Vector("x1", "y1", "z1"))
    } yield a + b

    writer1.value shouldBe 42
    writer1.written shouldBe Vector("a1", "b1", "c1", "x1", "y1", "z1")

    // you can also map the logs (for whatever reason)
    val upperCaseWriter = writer1.mapWritten(_.map(_.toUpperCase()))

    val result = upperCaseWriter.run

    result._1 shouldBe Vector("A1", "B1", "C1", "X1", "Y1", "Z1")

  }

  it("Can show work in process (Exercise 4.7.3") {

    def slowly[A](body: => A) =
      try body
      finally Thread.sleep(100)

    def factorial(n: Int): Int = {
      val ans = slowly(if (n == 0) 1 else n * factorial(n - 1))
      println(s"fact $n $ans")
      ans
    }

    def factLogged(n: Int): Logged[Int] = {
      for {
        ans <- if (n == 0) {
          1.pure[Logged]
        } else {
          slowly(factLogged(n - 1).map(_ * n))
        }
        _ <- Vector(s"fact $n $ans").tell
      } yield ans
    }

    val x = factLogged(5).run

    println(x)
  }
}
