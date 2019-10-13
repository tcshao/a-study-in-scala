package com.ocp.study.monad.definition

import org.scalatest.{FunSpec, Matchers}
import cats.data.Reader
import cats.syntax.applicative._ // for pure

class CatsReader extends FunSpec with Matchers {

  case class Cat(name: String, favoriteFood: String)

  it("Here is an example") {

    val catName: Reader[Cat, String] = Reader(cat => cat.name)

    catName.run(Cat("Garfield", "lasagne")) shouldBe "Garfield"
  }

  it("can be composable") {

    val catName: Reader[Cat, String] = Reader(cat => cat.name)

    val greetKitty: Reader[Cat, String] = catName.map(name => s"Hello ${name}")

    val feedKitty: Reader[Cat, String] = Reader(cat => s"Have a nice bowl of ${cat.favoriteFood}")

    val greetAndFeed: Reader[Cat, String] =
      for {
        greet <- greetKitty
        feed <- feedKitty
      } yield s"$greet. $feed."

    greetAndFeed(Cat("Garfield", "lasagne")) shouldBe "Hello Garfield. Have a nice bowl of lasagne."
  }

  it("Can deal with config options (Exercise 4.8.3)") {

    case class Db(
      usernames: Map[Int, String],
      passwords: Map[String, String]
    )

    type DbReader[A] = Reader[Db, A]

    def findUsername(userId: Int): DbReader[Option[String]] =
      Reader(db => db.usernames.get(userId))

    def checkPassword(username: String, password: String): DbReader[Boolean] =
      Reader(db => db.passwords.get(username).contains(password))

    def checkLogin(userId: Int, password: String): DbReader[Boolean] =
      for {
        maybeUserName <- findUsername(userId)
        isCorrect <- maybeUserName.map { name => checkPassword(name, password) }.getOrElse { false.pure[DbReader] }
      } yield isCorrect

      val users = Map(
        1 -> "dade",
        2 -> "kate",
        3 -> "margo"
      )
      
      val passwords = Map(
        "dade"  -> "zerocool",
        "kate"  -> "acidburn",
        "margo" -> "secret"
      )

      val db = Db(users, passwords)

      checkLogin(1, "zerocool").run(db) shouldBe true 
      checkLogin(4, "davinci").run(db) shouldBe false

  }

}
