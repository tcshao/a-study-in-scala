package com.ocp.study.typeclass.exercise

import org.scalatest.{FunSpec, Matchers}

import cats.Show
import cats.instances.int._    // for Show
import cats.instances.string._ // for Show
import cats.syntax.show._      // for show

class PrintableLibrarySpec extends FunSpec with Matchers {

  import PrintableLibrary.PrintableInstances._
  import PrintableLibrary.PrintableSyntax._
  import PrintableLibrary._

  it("Should use the Printable typeclass for a string") {

    val result = "HelloThere".toFormat

    result shouldBe ("Formatted -> HelloThere <-")
  }

  it("Should use the Printable typeclass for an int") {

    val result = 343.toFormat

    result shouldBe ("Formatted Int -> 343 <-")
  }

  it("Should dumpt to stdout") {
    495.toPrint
  }

  it("Can print out a cat") {
    val cat = Cat("Bill", 34, "Blue")

    val result = cat.toFormat

    result shouldBe ("Bill is a 34 year-old Blue cat.")
  }
}