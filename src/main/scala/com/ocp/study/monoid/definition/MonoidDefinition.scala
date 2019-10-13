package com.ocp.study.monoid.definition

object MonoidDefinition {

  // a monoid is a combination of a 
  // a SemiGroup -> ie a set together with a associative binary operation
  //      -> Associative -> numbers can be combined in different order and get the same result , ie (1 + 3) + 5 = 1 + (3 + 5)
  // an Empty Element -> ie what to combine to when starting with Nothing
  // the monoid for addition of integers has an Empty of 0, ie 0... 0 + 2 + 4 = 4 + 2 + 0
  // the monoid for multiplication of integers has an Empty of 1, ie = 1 * 2 * 5 = 5 * 2 * 1

  // so a trait for this would look like so
  trait SemiGroup[A] {
    def combine(x: A, y: A): A // if this was addition for ints an implementation would be x + y
  }

  trait Monoid[A] {
    def empty: A // if this was for addition the empty would be 0
  }
 }