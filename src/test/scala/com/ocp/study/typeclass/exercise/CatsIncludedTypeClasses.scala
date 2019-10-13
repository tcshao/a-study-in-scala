package com.ocp.study.typeclass.exercise

import org.scalatest.{FunSpec, Matchers}

import cats._
import cats.implicits._

class CatsIncludedSpec extends FunSpec with Matchers {

  import PrintableLibrary._

  it("Should also do the same thing with the cats library Show Method") {

    implicit val catShow: Show[Cat] =
      Show.show(cat => s"${cat.name} is a ${cat.age} year-old ${cat.color} cat.")

    val result = Cat("Bill", 34, "Blue").show

    result shouldBe ("Bill is a 34 year-old Blue cat.")
  }

  it("Should compare cats using the Cats Eq Method") {

    implicit val catEq: Eq[Cat] =
      Eq.instance[Cat] { (leftCat, rightCat) =>
        leftCat.name == rightCat.name && leftCat.age == rightCat.age && leftCat.color == rightCat.color
      }

    val cat1 = Cat("Bill", 34, "Blue")
    val cat2 = Cat("Frank", 22, "Green")

    val copyOfCat1 = cat1.copy()

    catEq.eqv(cat1, copyOfCat1) shouldBe true
    catEq.eqv(cat1, cat2) shouldBe false

    import cats.syntax.eq._

    (cat1 =!= cat2) shouldBe true

    val optionCat1 = Option(cat1)
    val optionCat2 = Option.empty[Cat]

    (optionCat1 =!= optionCat2) shouldBe true
  }
}
