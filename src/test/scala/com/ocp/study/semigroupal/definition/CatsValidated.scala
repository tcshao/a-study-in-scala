package com.ocp.study.semigroupal.exercise
import cats.Semigroupal
import cats.data.Validated

import cats.instances.list._ // for Monoid
import cats.syntax.either._ // for catchOnly
import org.scalatest.{FunSpec, Matchers}

class CatsValidated extends FunSpec with Matchers {
  it("Has a validated!") {

    type AllErrorsOr[A] = Validated[List[String], A]

    val res = Semigroupal[AllErrorsOr].product(
      Validated.invalid(List("Error 1")),
      Validated.invalid(List("Error 2"))
    )

    res shouldBe Validated.Invalid(List("Error 1", "Error 2"))
  }

  it("has extension methods to encapsulate other things") {

    Validated.catchOnly[NumberFormatException]("foo".toInt) shouldBe a[Validated.Invalid[_]]

    Validated.catchNonFatal(sys.error("Badness")) shouldBe a[Validated.Invalid[_]]

    Validated.fromTry(scala.util.Try("foo".toInt)) shouldBe a[Validated.Invalid[_]]

    Validated.fromEither[String, Int](Left("Badness")) shouldBe a[Validated.Invalid[_]]

    Validated.fromOption[String, Int](None, "Badness") shouldBe a[Validated.Invalid[_]]
  }

  it("Can validate a form Exercise (6.4.4)") {

    import cats.instances.list._ // for Semigroupal
    import cats.syntax.apply._ // for mapN

    case class User(name: String, age: Int)
    
    type FormData = Map[String, String]
    type FailFast[A] = Either[List[String], A]
    type FailSlow[A] = Validated[List[String], A]

    def getValue(name: String)(data: FormData): FailFast[String] =
      data.get(name).toRight(List(s"$name field not specified!"))

    def parseInt(name: String)(data: String): FailFast[Int] =
      Either
        .catchOnly[NumberFormatException](data.toInt)
        .leftMap(_ => List(s"$name must be an integer!"))

    def nonBlank(name: String)(data: String): FailFast[String] =
      Right(data).ensure(List(s"$name cannot be blank"))(_.nonEmpty)

    def nonNegative(name: String)(data: Int): FailFast[Int] =
      Right(data).ensure(List(s"$name must be non-negative"))(_ >= 0)

    def readName(data: FormData): FailFast[String] =
      getValue("name")(data).flatMap(nonBlank("name"))

    def readAge(data: FormData): FailFast[Int] =
      getValue("age")(data).flatMap(nonBlank("age")).flatMap(parseInt("age")).flatMap(nonNegative("age"))

    def readUser(data: FormData): FailSlow[User] =
      (
        readName(data).toValidated,
        readAge(data).toValidated
      ).mapN(User.apply)
  }
}
