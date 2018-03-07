package com.scalaua.services

import java.time.Instant

import org.scalatest.{FlatSpec, Matchers}

class FlipStateSpec extends FlatSpec with Matchers {

  "flip" should "payout win" in {
    implicit val rng: Rng = Rng.fixed(0)
    implicit val ts: Instant = Instant.now()

    var simulator = FlipPlayerSimulator("AAA", FlipState.initial, FlipActorProps("playerA"))

    simulator.status shouldBe "BetsAwaiting"
    simulator = simulator.flipCoin(3, FlipHead).right.get
    simulator.state.bet shouldBe Some(FlipBet(3, FlipHead))
    simulator.status shouldBe "CollectingBets"

    simulator = simulator.confirm
    simulator.status shouldBe "PayingOut"
    simulator.state.result shouldBe Some(FlipResult(FlipHead, 6))

    simulator = simulator.confirm
    simulator.status shouldBe "RoundFinished"

    simulator = simulator.startNewRound().right.get
    simulator.status shouldBe "BetsAwaiting"
    simulator.state.roundId shouldBe 1
    simulator.state.bet shouldBe None
    simulator.state.result shouldBe None
  }

}
