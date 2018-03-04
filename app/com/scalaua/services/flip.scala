package com.scalaua.services

import java.time.Instant

import scala.util.Random

//model

case class WalletConfirmation(newBalance: Int)

case class WalletError4xx()

case class WalletFailure5xx()

case class FlipError()

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

case class PendingRequest()

case object BetsAwaiting extends FlipStatus

case class CollectingBets(pendingRequest: PendingRequest) extends FlipStatus

case class PayingOut(pendingRequest: PendingRequest) extends FlipStatus

sealed trait FlipOutcome

case object FlipHead extends FlipOutcome

case object FlipTail extends FlipOutcome

case class FlipResult(outcome: FlipOutcome, win: Int)

case class FlipActorProps()

sealed trait FlipBehaviour {
  def handleCommand(state: FlipState)(rng: Random, props: FlipActorProps, ts: Instant): PartialFunction[Any, Either[FlipError, FlipEvent]]

  def handleEvent(state: FlipState)(props: FlipActorProps): PartialFunction[Any, FlipState]
}

case class FlipState(roundId: Long,
                     status: FlipStatus,
                     bet: Option[Int] = None,
                     result: Option[FlipResult] = None) {

}