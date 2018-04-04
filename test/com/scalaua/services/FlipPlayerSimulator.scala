package com.scalaua.services

import java.time.Instant

import scala.util.Right

case class FlipPlayerSimulator(state: FlipState, props: FlipActorProps) {

  def startNewRound()(implicit rng: Rng, ts: Instant): Either[FlipError, FlipPlayerSimulator] = {
    val command = StartNewRound()
    state.behaviour.handleCommand(rng, props, ts)(command) match {
      case Right(event) => Right(copy(state = state.behaviour.handleEvent(props)(event.head)))
      case Left(error) => Left(error)
    }
  }

  def attach(session: String)(implicit rng: Rng, ts: Instant): Either[FlipError, FlipPlayerSimulator] = {
    val command = Attach(session)
    state.behaviour.handleCommand(rng, props, ts)(command) match {
      case Right(event) => Right(copy(state = state.behaviour.handleEvent(props)(event.head)))
      case Left(error) => Left(error)
    }
  }

  def detach()(implicit rng: Rng, ts: Instant): Either[FlipError, FlipPlayerSimulator] = {
    val command = Detach()
    state.behaviour.handleCommand(rng, props, ts)(command) match {
      case Right(event) => Right(copy(state = state.behaviour.handleEvent(props)(event.head)))
      case Left(error) => Left(error)
    }
  }

  def flipCoin(bet: Int, alternative: String)(implicit rng: Rng, ts: Instant): Either[FlipError, FlipPlayerSimulator] = {
    val command = FlipCoin(bet, alternative)
    state.behaviour.handleCommand(rng, props, ts)(command) match {
      case Right(event) => Right(copy(state = state.behaviour.handleEvent(props)(event.head)))
      case Left(error) => Left(error)
    }
  }

  def confirm(implicit rng: Rng, ts: Instant): FlipPlayerSimulator = state.pendingRequest match {
    case Some(pendingRequest) =>
      val pr = pendingRequest.walletRequest
      val command = WalletConfirmation(pr.id, pr.amount, 0, ts)
      val event = state.behaviour.handleCommand(rng, props, ts)(command).right.get
      copy(state = state.behaviour.handleEvent(props)(event.head))
    case None =>
      this
  }

  def error(code: String)(implicit rng: Rng, ts: Instant): FlipPlayerSimulator = state.pendingRequest match {
    case Some(_) =>
      val command = WalletError4xx(code)
      val event = state.behaviour.handleCommand(rng, props, ts)(command).right.get
      copy(state = state.behaviour.handleEvent(props)(event.head))
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
