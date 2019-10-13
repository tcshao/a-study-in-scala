package com.ocp.study.monoid.exercise 

import org.scalatest.{FunSpec, Matchers}
import cats.Monoid

class CatsMonoids extends FunSpec with Matchers {

  it("should combine strings") {

    import cats.instances.string._

    val helloWorld = Monoid[String].combine("Hello, ", "World")
    val helloWithApply = Monoid.apply[String].combine("Hello, ", "World")

    helloWorld should equal(helloWithApply)
    helloWorld shouldBe "Hello, World"
  }

  it("should combine ints") {

    import cats.instances.int._

    val combineAdds = Monoid[Int].combine(34, 20)

    combineAdds shouldBe 54
  }

  it("It also can combine stuff in contexts, in this case Options") {

    import cats.instances.int._
    import cats.instances.option._

    val opt1 = Option(22)
    val opt2 = Option(1)
    val none: Option[Int] = None

    val combined = Monoid[Option[Int]].combine(opt1, opt2)

    combined shouldBe Some(23)

    // note combining options i guess does not map contexts, so combining with none still gives you a
    // value
    Monoid[Option[Int]].combine(opt1, none) shouldBe Some(22)
    Monoid[Option[Int]].combineAll(List(opt1, opt2, none)) shouldBe Some(23)
  }

  it("Also supports the tie fighter operator") {

    import cats.instances.string._ // for Monoid
    import cats.syntax.semigroup._ // for |+|

    val stringResult = "Hello" |+| " World" |+| Monoid[String].empty

    stringResult shouldBe "Hello World"
  }

  it("Should add numbers together (Exercise 2.5.4)") {

    import cats._
    import cats.implicits._
    
    def add1(items: List[Int]): Int = items.foldLeft(Monoid[Int].empty) { _ |+| _ }
    def add(items: List[Int]): Int = Monoid[Int].combineAll(items)

    val result = add(List(1,2,3))

    add(List(1,2,3)) should equal(add1(List(1,2,3)))

    result shouldBe 6
  }

  it("Should add numbers together with option (Exercise 2.5.4)") {

    import cats._
    import cats.implicits._
    
    // so if we take an implicit scala will see we are pulling in a type and try to reconcile that with the cats implicits
    def add[A](items: List[A])(implicit monoid: Monoid[A]): A = items.foldLeft(monoid.empty) { _ |+| _ }

    // using the context bounds syntax, syntactic suguar for that implicit
    def add2[A: Monoid](items: List[A]): A = items.foldLeft(Monoid[A].empty) { _ |+| _ }

    val optionList: List[Option[Int]] = List(Some(1),Some(2),Some(3))

    val result = add(optionList)
    
    result shouldBe Some(6)
    add(optionList) should equal(add2(optionList))
  }

  it("Should add together case classes (Exercise 2.5.4)") {

    import cats._
    import cats.implicits._
    
    def add[A: Monoid](items: List[A]): A = items.foldLeft(Monoid[A].empty) { _ |+| _ }

    case class Order(totalCost: Double, quantity: Double)

    val orders = List(
      Order(1,1),
      Order(3,3),
      Order(5,6)
    )

    implicit val orderMonoid: Monoid[Order] =
      new Monoid[Order] {
        def combine(x: Order, y: Order): Order = Order(x.totalCost + y.totalCost, x.quantity + y.quantity)
        def empty: Order = Order(0, 0)
      }

    val result = add(orders)
    
    result shouldBe Order(9, 10)
    (Order(1,1) |+| Order(1,1)) shouldBe Order(2,2)
  }

}