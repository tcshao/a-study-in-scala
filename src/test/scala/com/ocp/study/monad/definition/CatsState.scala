package com.ocp.study.monad.definition

import org.scalatest.{FunSpec, Matchers}
import cats.data.Reader
import cats.syntax.applicative._ // for pure
import cats.data.State
import cats._
import cats.implicits._

class CatsState extends FunSpec with Matchers {

  it("can process a tree (Exercise 4.10.1)") {

    sealed trait Tree[+A]

    final case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

    final case class Leaf[A](value: A) extends Tree[A]

    def branch[A](left: Tree[A], right: Tree[A]): Tree[A] =
      Branch(left, right)

    def leaf[A](value: A): Tree[A] =
      Leaf(value)

      val result = List(1,2,3) >>= (x => List(x, x + 1))

      result shouldBe List(1, 2, 2, 3, 3, 4)

  }

}
