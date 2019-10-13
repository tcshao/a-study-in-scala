package com.ocp.study.monadTransformers.definition

import org.scalatest.{FunSpec, Matchers}

import scala.concurrent.{Await, Future}
import cats._
import cats.implicits._
import cats.data._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._

class CatsMonadTransformers extends FunSpec with Matchers {

  it("should Transform! (Exercise 5.4)") {

    type Response[A] = EitherT[Future, String, A]

    val powerLevels = Map(
      "Jazz" -> 6,
      "Bumblebee" -> 8,
      "Hot Rod" -> 10
    )

    def getPowerLevel(autobot: String): Response[Int] =
      powerLevels.get(autobot) match {
        case Some(powerLevel) => EitherT.right(Future(powerLevel))
        case None             => EitherT.left(Future(s"$autobot unreachable"))
      }

    def canSpecialMove(ally1: String, ally2: String): Response[Boolean] =
      for {
        firstDude <- getPowerLevel(ally1)
        secondDude <- getPowerLevel(ally2)
      } yield (firstDude + secondDude) > 15

    def tacticalReport(ally1: String, ally2: String): String = {
      Await.result(canSpecialMove(ally1, ally2).value, 1.second) match {
        case Right(true)  => s"$ally1 and $ally2 can special"
        case Right(false) => s"$ally1 and $ally2 can not special"
        case Left(err)    => err
      }
    }

    Await.result(getPowerLevel("Jazz").value, 1.second) shouldBe Right(6)

    Await.result(canSpecialMove("Jazz", "Bob").value, 1.second) shouldBe Left("Bob unreachable")

    tacticalReport("Jazz", "Bumblebee") shouldBe "Jazz and Bumblebee can not special"
    tacticalReport("Bumblebee", "Hot Rod") shouldBe "Bumblebee and Hot Rod can special"
  }

}
