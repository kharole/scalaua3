package com.scalaua.services

import java.time.Instant

import org.scalatest.{FlatSpec, Matchers}

import scala.util.Random

class FlipStateSpec extends FlatSpec with Matchers {

  "flip" should "payout win" in {
    implicit val rng = new Random()
    implicit val ts = Instant.now()

    var simulator = FlipPlayerSimulator("playerA", "AAA", FlipState(0, BetsAwaiting), FlipActorProps())

    simulator = simulator.flip(3).right.get
    simulator.state.bet shouldBe Some(3)
    simulator.state.status shouldBe "BetsAwaiting"
  }

}
