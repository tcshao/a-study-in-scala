package com.ocp.study.typeclass.definition

object TypeClassDefinition {
  // typeclasses allow us to augment a given type.  we can code various behavior and put this 'armor' on other types if we wish
  // The first step in creating one is defining the API of it, which is illustrated here with an AST and a trait

  // the AST, the "parts" of jason
  sealed trait Json
  final case class JsObject(get: Map[String, Json]) extends Json
  final case class JsString(get: String) extends Json
  final case class JsNumber(get: Double) extends Json
  case object JsNull extends Json

  // The Interface to the behavior
  trait JsonWriter[A] {
    def write(value: A): Json
  }

  // now we have to create instances of this type class ie for a type we have to create the implementation of
  // JsonWriter

  final case class Person(name: String, email: String)

  object JsonWriterInstances {
    // this is what it will do when you call JsonWriter on a String type
    implicit val stringWriter: JsonWriter[String] =
      new JsonWriter[String] {
        def write(value: String): Json = JsString(value)
      }

    // this is what it will do when you call JsonWriter on a Person type
    implicit val personWriter: JsonWriter[Person] =
      new JsonWriter[Person] {
        def write(value: Person): Json =
          JsObject(
            Map(
              "name" -> JsString(value.name),
              "email" -> JsString(value.email)
            )
          )
      }

    // the above are vals because the explicit type "String, Person" is required
    // if the type is not known, or it is recursively derived we need a def
    implicit def optionWriter[A](implicit writer: JsonWriter[A]): JsonWriter[Option[A]] =
      new JsonWriter[Option[A]] {
        def write(option: Option[A]): Json =
          option match {
            case Some(value) => writer.write(value)
            case None        => JsNull
          }
      }

    // ok now that we have the API definined and instances we can make the syntax for it
  }

  object JsonSyntax {
    implicit class JsonWriterOps[A](value: A) { // implicit class..aka extension method, tack this on to everything
      def toJson(implicit w: JsonWriter[A]): Json =
        w.write(value) // implicitly pull out the writer based upon what object we give this.
    }
  }
}
