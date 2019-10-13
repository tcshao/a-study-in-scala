package com.cop.study.monad.definition

import org.scalatest.{FunSpec, Matchers}
import cats.syntax.either._

class CatsEither extends FunSpec with Matchers {
  it("works diffenently then the included Either") {

    3.asRight[String] shouldBe Right(3)
    4.asRight[String] shouldBe a[Either[_, _]]
  }

  it("can be combined") {

    def countPositive(nums: List[Int]) =
      nums.foldLeft(0.asRight[String]) { (accumulator, num) =>
        if (num > 0) {
          accumulator.map(_ + 1)
        } else {
          Left("Negative. Stopping!")
        }
      }

    countPositive(List(1,2,3)) shouldBe Right(3)
    countPositive(List(1,2,3,4,-3)) shouldBe Left("Negative. Stopping!")
  }

  it("can be used with exceptions") {

    val either = Either.catchOnly[NumberFormatException]("foo".toInt)

    either.isLeft shouldBe true
    either.left.get shouldBe a [NumberFormatException]
  }
}
