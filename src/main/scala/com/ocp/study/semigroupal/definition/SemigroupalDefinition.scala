package com.ocp.study.semigroupal.definition

object SemigroupalDefinition {
  // Semigroupal encompasses the notion of composing pairs of contexts. 
  // Cats provides a cats.syntax.apply module that makes use of Semigroupal and Functor to allow users to sequence functions with multiple arguments.
  trait Semigroupal[F[_]] {
    def product[A, B](fa: F[A], fb: F[B]): F[(A, B)]
  }

  
}