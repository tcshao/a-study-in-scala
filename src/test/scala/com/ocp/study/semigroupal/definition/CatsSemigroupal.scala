package com.ocp.study.semigroupal.exercise

import org.scalatest.{FunSpec, Matchers}
import cats.Semigroupal
import cats.instances.option._ // for Semigroupal
import cats.Monoid
import cats.instances.int._ // for Monoid
import cats.instances.invariant._ // for Semigroupal
import cats.instances.list._ // for Monoid
import cats.instances.string._ // for Monoid
import cats.syntax.apply._ // for imapN
import cats.syntax.semigroup._ // for |+|

class CatsSemigroupal extends FunSpec with Matchers {
  it("is a semigroupal!") {
    // joining 2 contexts
    Semigroupal[Option].product(Some(123), Some("abc")) shouldBe Some((123, "abc"))

    // joining 3 contexts
    Semigroupal.tuple3(Option(1), Option(2), Option(3)) shouldBe Some((1, 2, 3))

    // they can be mapped
    Semigroupal.map3(Option(1), Option(2), Option(3))(_ + _ + _) shouldBe Some(6)

    import cats.syntax.apply._ // for tupled and mapN

    // companion object has some other methods to make this more concise
    (Option(123), Option("abc")).tupled shouldBe Some((123, "abc"))

    case class Cat(name: String, born: Int, color: String)

    (Option("Garfield"), Option(1978), Option("Orange & black")).mapN(Cat.apply) shouldBe Some(
      Cat("Garfield", 1978, "Orange & black")
    )
  }

  it("can combine multiple things while creating objects") {

    case class Cat(
      name: String,
      yearOfBirth: Int,
      favoriteFoods: List[String]
    )

    val tupleToCat: (String, Int, List[String]) => Cat = Cat.apply _

    val catToTuple: Cat => (String, Int, List[String]) = cat => (cat.name, cat.yearOfBirth, cat.favoriteFoods)

    implicit val catMonoid: Monoid[Cat] = (
      Monoid[String],
      Monoid[Int],
      Monoid[List[String]]
    ).imapN(tupleToCat)(catToTuple)

    val garfield = Cat("Garfield", 1978, List("Lasagne"))
    val heathcliff = Cat("Heathcliff", 1988, List("Junk Food"))

    (garfield |+| heathcliff) shouldBe Cat("GarfieldHeathcliff",3966,List("Lasagne", "Junk Food"))
  }

}
