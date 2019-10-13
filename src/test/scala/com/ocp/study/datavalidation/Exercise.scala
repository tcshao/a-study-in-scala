package com.ocp.study.datavalidation

import org.scalatest.{FunSpec, Matchers}
import cats.Semigroup
import cats.syntax.either._    // for asLeft and asRight
import cats.syntax.semigroup._ // for |+|

class DataValidationSpec extends FunSpec with Matchers {

  trait Check[E, A] {
    def apply(value: A): Either[E, A]
    def and(that: Check[E, A]): Check[E, A]
  }

  abstract class CheckF[E, A](func: A => Either[E, A]) {
    def apply(a: A): Either[E, A] = func(a)

    def and(that: CheckF[E, A])
  }

}