package com.scalaua.services

import java.time.Instant

import org.scalatest.{FlatSpec, Matchers}

import scala.util.Random

class FlipStateSpec extends FlatSpec with Matchers {

  "flip" should "payout win" in {
    implicit val rng: Random = new Random()
    implicit val ts: Instant = Instant.now()

    var simulator = FlipPlayerSimulator("AAA", FlipState.initial, FlipActorProps("playerA"))

    simulator.status shouldBe "BetsAwaiting"
    simulator = simulator.flipCoin(3).right.get
    simulator.state.bet shouldBe Some(3)
    simulator.status shouldBe "CollectingBets"
  }

}
