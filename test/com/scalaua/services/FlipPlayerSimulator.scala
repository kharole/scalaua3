package com.scalaua.services

import java.time.Instant

import scala.util.Right

case class FlipPlayerSimulator(state: FlipState, props: FlipActorProps) {

  def startNewRound()(implicit rng: Rng, ts: Instant): Either[FlipError, FlipPlayerSimulator] = {
    val command = StartNewRound()
    state.handleCommand(rng, props, ts)(command) match {
      case Right(event) => Right(copy(state = state.handleEvent(props)(event)))
      case Left(error) => Left(error)
    }
  }

  def attach(session: String): FlipPlayerSimulator = {
    copy(state = state.attach(session))
  }

  def detach(): FlipPlayerSimulator = {
    copy(state = state.detach())
  }

  def flipCoin(bet: Int, alternative: String)(implicit rng: Rng, ts: Instant): Either[FlipError, FlipPlayerSimulator] = {
    val command = FlipCoin(bet, alternative)
    state.handleCommand(rng, props, ts)(command) match {
      case Right(event) => Right(copy(state = state.handleEvent(props)(event)))
      case Left(error) => Left(error)
    }
  }

  def confirm(implicit rng: Rng, ts: Instant): FlipPlayerSimulator = state.pendingRequest match {
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
    case RoundFinished => "RoundFinished"
  }
}
