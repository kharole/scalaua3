package com.scalaua.services

import java.time.Instant

import scala.util.{Random, Right}

case class FlipPlayerSimulator(playerId: String, session: String, state: FlipState, props: FlipActorProps) {

  def flip(bet: Int)(implicit rng: Random, ts: Instant): Either[FlipError, FlipPlayerSimulator] = {
    val command = Flip(session, bet)
    state.handleCommand(rng, props, ts)(command) match {
      case Right(event) => Right(copy(state = state.handleEvent(props)(event)))
      case Left(error) => Left(error)
    }
  }

  def confirm(implicit rng: Random, ts: Instant): FlipPlayerSimulator = state.pendingRequest match {
    case Some(pendingRequest) =>
      val pr = pendingRequest.walletRequest
      val command = WalletConfirmation(pr.id, pr.amount, 0, ts)
      val event = state.handleCommand(rng, props, ts)(command).right.get
      copy(state = state.handleEvent(props)(event))
    case None =>
      this
  }

  def status: String = state.status match {
    case BetsAwaiting => "BetsAwaiting"
    case CollectingBets(_) => "CollectingBets"
    case PayingOut(_) => "PayingOut"
  }
}
