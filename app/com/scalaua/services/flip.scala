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

case class FlipError(code: String)

//commands

sealed trait FlipCommand

case class Attach(session: String) extends FlipCommand

case class Detach() extends FlipCommand

case class FlipCoin(bet: Int, alternative: CoinSide) extends FlipCommand

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

case class BetsAccepted(session: String, amount: Int, alternative: CoinSide, timestamp: Instant) extends FlipEvent

case class BetsConfirmed(confirmation: WalletConfirmation, outcome: CoinSide, timestamp: Instant) extends FlipEvent with ConfirmationEvent

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

case object RoundFinished extends FlipStatus

sealed trait CoinSide

case object FlipHead extends CoinSide

case object FlipTail extends CoinSide

case class FlipResult(outcome: CoinSide, win: Int)

case class FlipActorProps(playerId: String)

object FlipState {
  def initial = FlipState(0, BetsAwaiting)
}

case class FlipBet(amount: Int, alternative: CoinSide)

case class FlipState(roundId: Int,
                     status: FlipStatus,
                     bet: Option[FlipBet] = None,
                     result: Option[FlipResult] = None) {

  def pendingRequest: Option[PendingRequest] = status match {
    case BetsAwaiting => None
    case CollectingBets(p) => Some(p)
    case PayingOut(p) => Some(p)
    case RoundFinished => None
  }

  def placeBet(amount: Int, alternative: CoinSide): FlipState =
    copy(bet = Some(FlipBet(amount, alternative)))

  def gotoCollectingBets(session: String, ts: Instant)(implicit props: FlipActorProps): FlipState = {
    val p = PendingRequest(WalletRequest(s"${props.playerId}.$roundId", "BET", bet.get.amount, ts))
    copy(status = CollectingBets(p))
  }

  def handleCommand(implicit session: String, rng: Random, props: FlipActorProps, ts: Instant): PartialFunction[FlipCommand, Either[FlipError, FlipEvent]] =
    status match {
      case BetsAwaiting => BetsAwaitingBehaviour.handleCommand(this)
      case CollectingBets(_) => CollectingBetsBehaviour.handleCommand(this)
      case PayingOut(_) => PayingOutBehaviour.handleCommand(this)
      case RoundFinished => RoundFinishedBehaviour.handleCommand(this)
    }

  def handleEvent(props: FlipActorProps): PartialFunction[FlipEvent, FlipState] =
    status match {
      case BetsAwaiting => BetsAwaitingBehaviour.handleEvent(this)(props)
      case CollectingBets(_) => CollectingBetsBehaviour.handleEvent(this)(props)
      case PayingOut(_) => PayingOutBehaviour.handleEvent(this)(props)
      case RoundFinished => RoundFinishedBehaviour.handleEvent(this)(props)
    }

}

//behaviour
sealed trait FlipBehaviour {
  def handleCommand(state: FlipState)(implicit session: String, rng: Random, props: FlipActorProps, ts: Instant): PartialFunction[FlipCommand, Either[FlipError, FlipEvent]]

  def handleEvent(state: FlipState)(implicit props: FlipActorProps): PartialFunction[FlipEvent, FlipState]
}

object BetsAwaitingBehaviour extends FlipBehaviour {
  override def handleCommand(state: FlipState)(implicit session: String, rng: Random, props: FlipActorProps, ts: Instant): PartialFunction[FlipCommand, Either[FlipError, FlipEvent]] = {
    case FlipCoin(bet, alternative) =>
      if (bet > 0 && bet <= 5) {
        Right(BetsAccepted(session, bet, alternative, ts))
      } else {
        Left(FlipError("error.invalid.bet"))
      }
  }

  override def handleEvent(state: FlipState)(implicit props: FlipActorProps): PartialFunction[FlipEvent, FlipState] = {
    case BetsAccepted(session, amount, alternative, ts) =>
      state
        .placeBet(amount, alternative)
        .gotoCollectingBets(session, ts)
  }
}

object CollectingBetsBehaviour extends FlipBehaviour {
  override def handleCommand(state: FlipState)(implicit session: String, rng: Random, props: FlipActorProps, ts: Instant): PartialFunction[FlipCommand, Either[FlipError, FlipEvent]] = {
    case WalletConfirmation(id, amount, newBalance, processedAt) => ???

    case WalletError4xx(code) => ???

    case WalletFailure5xx(code) => ???
  }

  override def handleEvent(state: FlipState)(implicit props: FlipActorProps): PartialFunction[FlipEvent, FlipState] = {
    ???
  }
}

object PayingOutBehaviour extends FlipBehaviour {
  override def handleCommand(state: FlipState)(implicit session: String, rng: Random, props: FlipActorProps, ts: Instant): PartialFunction[FlipCommand, Either[FlipError, FlipEvent]] = ???

  override def handleEvent(state: FlipState)(implicit props: FlipActorProps): PartialFunction[FlipEvent, FlipState] = ???
}

object RoundFinishedBehaviour extends FlipBehaviour {
  override def handleCommand(state: FlipState)(implicit session: String, rng: Random, props: FlipActorProps, ts: Instant): PartialFunction[FlipCommand, Either[FlipError, FlipEvent]] = ???

  override def handleEvent(state: FlipState)(implicit props: FlipActorProps): PartialFunction[FlipEvent, FlipState] = ???
}