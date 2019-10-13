package com.ocp.study

import org.scalatest.{FunSpec, Matchers}

class ImplicitSpec extends FunSpec with Matchers {
  it("Implicitly will pick up any int declared in scope") {
    implicit val implicitInt = 5

    val result = implicitly[Int] + 6

    result should be(11)
  }

  it("Will look up the chain for the next value") {

    implicit val imString: String = "Hello"

    {
      val result = implicitly[String] + " Nurse!"

      result should be("Hello Nurse!")
    }
  }

  it("Will get passed to functions that have implicit params defined") {

    implicit val three: Int = 3

    def addThree(number: Int)(implicit shouldBeThree: Int): Int = number + shouldBeThree

    val result = addThree(5) 

    result should be(8)
  }
}
