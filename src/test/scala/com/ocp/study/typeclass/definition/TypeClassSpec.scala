package com.ocp.study.typeclass.definition

import org.scalatest.{FunSpec, Matchers}

class TypeClassSpec extends FunSpec with Matchers {

  import TypeClassDefinition.JsonWriterInstances._
  import TypeClassDefinition.JsonSyntax._
  import TypeClassDefinition._

  it("Should use the JsonWriter typeclass for a string") {

    val result = "HelloThere".toJson

    result shouldBe (JsString("HelloThere"))
  }

  it("Should use the JsonWriter typeclass for Person") {

    val json = Person("Dave", "Dave@example.com").toJson

    json should be(
      JsObject(
        Map("name" -> JsString("Dave"), "email" -> JsString("Dave@example.com"))
      )
    )
  }

  it("Should use the JsonWriter typeclass For Option") {

    val optionToJson = Option("Hey").toJson

    optionToJson shouldBe (JsString("Hey"))

    val noneToJson = Option.empty[String].toJson

    noneToJson shouldBe JsNull
  }

}
