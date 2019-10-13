package com.ocp.study.monadTransformers.definition

import cats.Monad
import cats.instances.list._     // for Monad
import cats.syntax.applicative._ // for pure
import cats.data.OptionT

object MonadTransformer {

  // monad transformers are the purpose to stack monads on top of eachother
  
  type ListOption[A] = OptionT[List, A] // Same as List[Option[A]]

  // The syntax is the Outermost defined higher kinded type becomes the innermost of the stack, 
  // like the thing above is making a List of Options  

  def creatingTransformers = {

    val result1 : ListOption[Int] = OptionT(List(Option(10)))

    val result2 : ListOption[Int] = 32.pure[ListOption] // Will be OptionT(List(Some(32)))
  }
}
