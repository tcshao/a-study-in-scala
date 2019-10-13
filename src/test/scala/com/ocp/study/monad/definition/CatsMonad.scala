package com.ocp.study.monad.definition

import cats.Monad
import cats.instances.option._ // for Monad
import cats.instances.list._ // for Monad
import org.scalatest.{FunSpec, Matchers}

class CatsMonad extends FunSpec with Matchers {
  it("exists and works") {

    Monad[Option].pure(3) shouldBe Some(3)

    Monad[Option].flatMap(Monad[Option].pure(3))(a => Some(a + 2)) shouldBe Some(5)

  }

  it("Implement Id (Exercise 4.3,1") {

    import cats.Id

    def pure[A](value: A): Id[A] = value
    
    def map[A, B](initial: Id[A])(func: A => B): Id[B] =
     func(initial) 
    
    def flatMap[A, B](initial: Id[A])(func: A => Id[B]): Id[B] = 
      func(initial)
  }
}
