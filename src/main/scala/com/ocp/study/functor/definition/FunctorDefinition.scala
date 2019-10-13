package com.ocp.study.functor.definition

import cats.instances.function._ // for Functor
import cats.syntax.functor._     // for map

object FunctorDefinition {

  // basically a functor is anything that supports the map method
  // it is important to note that Functor leaves the structore of the context unchanged
  // so after you run a map method you end up with the same context, it just could be a different data type inside
  // its important to note if you run a map method on a list you end up with the same number of items.

  // List[🐐] -> map (🐐 => 🦌) -> List[🦌]
  // Option[🍕] -> map (🍕-> 🍔) => Option[🍔]

  // also if a context has two parameterized types it will only change one

  // Either[🦌,🐐] => map (🐐 -> 🚓) => Either[🦌,🚓]

  val func1 = (x: Int) => x.toDouble

  val func2 = (y: Double) => y * 2

  func1.map(func2)(1)
  (func1 andThen func2)(1)

  // so the signiture of a functor is as follows
  import scala.language.higherKinds

  trait Functor[F[_]] { // the F is a type constructor with "one hole" or one type parameter, this is a stand in for any Type with one parameter 
                        // a type that is also parameterized is also referred to as a Higher Kinded Type
    def map[A, B](fa: F[A])(f: A => B): F[B]
  }


}