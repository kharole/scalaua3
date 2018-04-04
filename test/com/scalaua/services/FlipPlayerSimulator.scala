package com.scalaua.services

import java.time.Instant

import scala.util.Right

case class FlipPlayerSimulator(state: FlipState, props: FlipActorProps) {

  def startNewRound()(implicit rng: Rng, ts: Instant): Either[FlipError, FlipPlayerSimulator] = {
    validateAndApply(rng, ts, StartNewRound())
  }

  def attach(session: String)(implicit rng: Rng, ts: Instant): FlipPlayerSimulator = {
    validateAndApply(rng, ts, Attach(session)).right.get
  }

  def detach()(implicit rng: Rng, ts: Instant): FlipPlayerSimulator = {
    validateAndApply(rng, ts, Detach()).right.get
  }

  def flipCoin(bet: Int, alternative: String)(implicit rng: Rng, ts: Instant): Either[FlipError, FlipPlayerSimulator] = {
    validateAndApply(rng, ts, FlipCoin(bet, alternative))
  }

  def confirm(implicit rng: Rng, ts: Instant): FlipPlayerSimulator = state.pendingRequest match {
    case Some(pendingRequest) =>
      val pr = pendingRequest.walletRequest
      validateAndApply(rng, ts, WalletConfirmation(pr.id, pr.amount, 0, ts)).right.get
    case None =>
      this
  }

  def error(code: String)(implicit rng: Rng, ts: Instant): FlipPlayerSimulator = state.pendingRequest match {
    case Some(_) =>
      validateAndApply(rng, ts, WalletError4xx(code)).right.get
    case None =>
      this
  }

  private def validateAndApply(rng: Rng, ts: Instant, command: FlipCommand): Either[FlipError, FlipPlayerSimulator] = {
    state.behaviour.validateCommand(rng, props, ts)(command) match {
      case Right(event) => Right(copy(state = state.behaviour.handleEvent(props)(event.head)))
      case Left(error) => Left(error)
    }
  }

  def status: String = state.status match {
    case BetsAwaiting => "BetsAwaiting"
    case CollectingBets(_) => "CollectingBets"
    case PayingOut(_) => "PayingOut"
    case RoundFinished => "RoundFinished"
  }
}
