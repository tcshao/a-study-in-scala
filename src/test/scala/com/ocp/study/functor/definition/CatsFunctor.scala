package com.ocp.study.functor.definition

import org.scalatest.{FunSpec, Matchers}

import cats._
import cats.implicits._

class CatsFunctor extends FunSpec with Matchers {

  it("can be used to compose functions!") {

    val func1 = (a: Int) => a + 1
    val func2 = (a: Int) => a * 2
    val func3 = (a: Int) => a + "!"

    val func4 = func1.map(func2).map(func3) // functions mapping over functions makes functional composiition!
  }

  it("can be abstracted over whatever context we decide to give it") {

    def doMath[F[_]](start: F[Int])(implicit functor: Functor[F]): F[Int] =
      start.map(n => n + 1 * 2)

    doMath(Option(20)) shouldBe Some(22)

    doMath(List(1, 2, 3)) shouldBe List(3, 4, 5)
  }

  it("allows you to 'lift' other methods in the context of that functor") {

    val func = (x: Int) => x + 1

    val liftedFunc = Functor[Option].lift(func)

    liftedFunc(Option(3)) shouldBe Some(4)
  }

  it("can be used to create functors for other types it doesn't know about") {

    final case class Box[A](value: A)

    implicit val boxFunctor: Functor[Box] =
      new Functor[Box] {
        def map[A, B](value: Box[A])(func: A => B): Box[B] =
          value.copy(value = func(value.value))
      }

    val newBox = Box(4).map { _ + 3 }

    val stringBox = Box("Hello").map { _ + " World!" }

    newBox.value shouldBe 7
    stringBox.value shouldBe "Hello World!"
  }

  it("can support a tree (Exercise 3.5.4)") {

    sealed trait Tree[+A]

    final case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

    final case class Leaf[A](value: A) extends Tree[A]

    object Tree {
      def branch[A](left: Tree[A], right: Tree[A]): Tree[A] =
        Branch(left, right)

      def leaf[A](value: A): Tree[A] =
        Leaf(value)
    }

    implicit val treeFunctor: Functor[Tree] =
      new Functor[Tree] {
        def map[A, B](tree: Tree[A])(func: A => B): Tree[B] = tree match {
          case Branch(left, right) => Branch(map(left)(func), map(right)(func))
          case Leaf(v)             => Leaf(func(v))
        }
      }

    Tree.leaf(100).map(_ * 2) shouldBe Leaf(200)

    Tree.branch(Tree.leaf(10), Tree.leaf(20)).map(_ * 2) shouldBe Branch(Leaf(20), Leaf(40))
  }

  it("supports contramap, which is just like map, but it sequences functions before not after") {

    trait Printable[A] {
      self =>
      def format(value: A): String

      def contramap[B](func: B => A): Printable[B] =
        new Printable[B] {
          def format(value: B): String = self.format(func(value))
        }
    }

    def format[A](value: A)(implicit p: Printable[A]): String =
      p.format(value)

    implicit val stringPrintable: Printable[String] =
      new Printable[String] {
        def format(value: String): String =
          "\"" + value + "\""
      }

    implicit val booleanPrintable: Printable[Boolean] =
      new Printable[Boolean] {
        def format(value: Boolean): String =
          if (value) "yes" else "no"
      }

    format(true) shouldBe "yes"

    final case class Box[A](value: A)

    // now to make this printable..so the long way of doing this as we did before
    //implicit def boxPrintable[A](implicit p: Printable[A]) =
    //  new Printable[Box[A]] {
    //    def format(box: Box[A]): String =
    //      p.format(box.value)
    //  }

    // with contramap we can add a function before it gets processed
    implicit def boxPrintable2[A](implicit p: Printable[A]) =
      p.contramap[Box[A]](_.value)

    format(Box(true)) shouldBe "yes"
  }

  it("supports imap, which goes forward and backward between covariant and contravariant") {

    trait Codec[A] {
      self =>
      def encode(value: A): String
      def decode(value: String): A
      def imap[B](dec: A => B, enc: B => A): Codec[B] =
        new Codec[B] {
          def encode(value: B): String = self.encode(enc(value))
          def decode(value: String): B = dec(self.decode(value))
        }
    }

    def encode[A](value: A)(implicit c: Codec[A]): String =
      c.encode(value)

    def decode[A](value: String)(implicit c: Codec[A]): A =
      c.decode(value)

    implicit val stringCodec: Codec[String] =
      new Codec[String] {
        def encode(value: String): String = value
        def decode(value: String): String = value
      }

    implicit val intCodec: Codec[Int] =
      stringCodec.imap(_.toInt, _.toString)

    implicit val booleanCodec: Codec[Boolean] =
      stringCodec.imap(_.toBoolean, _.toString)

    // creating a codec for double
    implicit val doubleCodec: Codec[Double] =
      stringCodec.imap[Double](_.toDouble, _.toString)

    case class Box[A](value: A)

    implicit def boxCodec[A](implicit c: Codec[A]) =
      c.imap[Box[A]](Box(_), _.value)

    // lets see if this works
    decode[Box[Double]]("123.4") shouldBe(Box(123.4))
    encode[Box[Int]](Box(23)) shouldBe("23")
  }
}
