package com.scalaua.services

import java.time.Instant

import scala.util.Random

//model

case class PendingRequest(walletRequest: WalletRequest, nrOfAttempts: Int = 0, undelivered: Boolean = false) {
  def incNrOfAttempts: PendingRequest = copy(nrOfAttempts = nrOfAttempts + 1)

  def resetNrOfAttempts: PendingRequest = copy(nrOfAttempts = 0)

  def undeliver: PendingRequest = copy(undelivered = true)

  def resetUndeliver: PendingRequest = copy(undelivered = false)

  def delayMultiplier: Long = if (nrOfAttempts > 0) pow2to(nrOfAttempts - 1) else 0L

  private def pow2to(n: Int): Long = 1L << n
}

case class WalletRequest(id: String, requestType: String, amount: Int, ts: Instant)

sealed trait WalletResponse extends FlipCommand

case class WalletConfirmation(id: String, amount: Int, newBalance: Int, processedAt: Instant) extends WalletResponse

case class WalletError4xx(code: String) extends WalletResponse

case class WalletFailure5xx(code: String) extends WalletResponse

case class FlipError()

//commands

sealed trait FlipCommand

case class Flip(session: String, bet: Int) extends FlipCommand

//events
trait Event {
  val timestamp: Instant
}

sealed trait FlipEvent extends Event {
  def newBalance: Option[Int] = this match {
    case rc: ConfirmationEvent =>
      Some(rc.confirmation.newBalance)
    case _ =>
      None
  }
}

trait FlipWalletError {
  val reason: WalletError4xx
}

trait FlipAttemptFailed {
  val reason: WalletFailure5xx
}


trait ConfirmationEvent {
  val confirmation: WalletConfirmation
}

case class BetsAccepted(sessionKey: String, bet: Int, timestamp: Instant) extends FlipEvent

case class BetsConfirmed(confirmation: WalletConfirmation, outcome: FlipOutcome, timestamp: Instant) extends FlipEvent with ConfirmationEvent

case class BetError(reason: WalletError4xx, timestamp: Instant) extends FlipEvent with FlipWalletError

case class BetAttemptFailed(reason: WalletFailure5xx, timestamp: Instant) extends FlipEvent with FlipAttemptFailed

case class WinConfirmed(confirmation: WalletConfirmation, timestamp: Instant) extends FlipEvent with ConfirmationEvent

case class WinRefused(reason: WalletError4xx, timestamp: Instant) extends FlipEvent with FlipWalletError

case class WinAttemptFailed(reason: WalletFailure5xx, timestamp: Instant) extends FlipEvent with FlipAttemptFailed

case class NewRoundStarted(timestamp: Instant) extends FlipEvent

//state
sealed trait FlipStatus

case object BetsAwaiting extends FlipStatus

case class CollectingBets(pendingRequest: PendingRequest) extends FlipStatus

case class PayingOut(pendingRequest: PendingRequest) extends FlipStatus

sealed trait FlipOutcome

case object FlipHead extends FlipOutcome

case object FlipTail extends FlipOutcome

case class FlipResult(outcome: FlipOutcome, win: Int)

case class FlipActorProps()

case class FlipState(roundId: Int,
                     status: FlipStatus,
                     bet: Option[Int] = None,
                     result: Option[FlipResult] = None) {

  def pendingRequest: Option[PendingRequest] = status match {
    case BetsAwaiting => None
    case CollectingBets(p) => Some(p)
    case PayingOut(p) => Some(p)
  }

  def handleCommand(rng: Random, props: FlipActorProps, ts: Instant): PartialFunction[FlipCommand, Either[FlipError, FlipEvent]] =
    status match {
      case BetsAwaiting => BetsAwaitingBehaviour.handleCommand(this)(rng, props, ts)
      case CollectingBets(_) => CollectingBetsBehaviour.handleCommand(this)(rng, props, ts)
      case PayingOut(_) => PayingOutBehaviour.handleCommand(this)(rng, props, ts)
    }

  def handleEvent(props: FlipActorProps): PartialFunction[FlipEvent, FlipState] =
    status match {
      case BetsAwaiting => BetsAwaitingBehaviour.handleEvent(this)(props)
      case CollectingBets(_) => CollectingBetsBehaviour.handleEvent(this)(props)
      case PayingOut(_) => PayingOutBehaviour.handleEvent(this)(props)
    }

}

//behaviour
sealed trait FlipBehaviour {
  def handleCommand(state: FlipState)(rng: Random, props: FlipActorProps, ts: Instant): PartialFunction[FlipCommand, Either[FlipError, FlipEvent]]

  def handleEvent(state: FlipState)(props: FlipActorProps): PartialFunction[FlipEvent, FlipState]
}

object BetsAwaitingBehaviour extends FlipBehaviour {
  override def handleCommand(state: FlipState)(rng: Random, props: FlipActorProps, ts: Instant): PartialFunction[FlipCommand, Either[FlipError, FlipEvent]] =
ª    ???

  override def handleEvent(state: FlipState)(props: FlipActorProps): PartialFunction[FlipEvent, FlipState] =
    ???
}

object CollectingBetsBehaviour extends FlipBehaviour {
  override def handleCommand(state: FlipState)(rng: Random, props: FlipActorProps, ts: Instant): PartialFunction[FlipCommand, Either[FlipError, FlipEvent]] = ???

  override def handleEvent(state: FlipState)(props: FlipActorProps): PartialFunction[FlipEvent, FlipState] = ???
}

object PayingOutBehaviour extends FlipBehaviour {
  override def handleCommand(state: FlipState)(rng: Random, props: FlipActorProps, ts: Instant): PartialFunction[FlipCommand, Either[FlipError, FlipEvent]] = ???

  override def handleEvent(state: FlipState)(props: FlipActorProps): PartialFunction[FlipEvent, FlipState] = ???
}