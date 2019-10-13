package com.ocp.study.typeclass.exercise

object PrintableLibrary {

  final case class Cat(name: String, age: Int, color: String)

  // The Interface to the behavior
  trait Printable[A] {
    def format(value: A): String
  }

  // the instances
  object PrintableInstances {

    implicit val printableString: Printable[String] =
      new Printable[String] {
        def format(value: String): String = s"Formatted -> $value <-"
      }

    implicit val printableInt: Printable[Int] =
      new Printable[Int] {
        def format(value: Int): String = s"Formatted Int -> $value <-"
      }

    implicit val printableCat: Printable[Cat] =
      new Printable[Cat] {
        def format(value: Cat): String = s"${value.name} is a ${value.age} year-old ${value.color} cat." 
      }
  }

  // the syntax
  object PrintableSyntax {

    implicit class PrintableOps[A](value: A) { 
      def toFormat(implicit w: Printable[A]): String =
        w.format(value)

      def toPrint(implicit w: Printable[A]): Unit =
        println(w.format(value))
    }
  }
}
