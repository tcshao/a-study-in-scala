package com.ocp.study.monoid.definition

object MonadDefinition {
  // a monad is a way of sequencing computations
  // it is defined by two methods, one for taking a value and bringing it into a context
  // the other is a flatmap which tells it to move on to the next step

  import scala.language.higherKinds

  trait Monad[F[_]] {
    def pure[A](value: A): F[A]
    def flatMap[A, B](value: F[A])(func: A => F[B]): F[B]

    // Exercise 4.1.2 Trying to define map
    def map[A, B](value: F[A])(func: A => B): F[B] =
      flatMap(value)(x => pure(func(x)))
  }
}
